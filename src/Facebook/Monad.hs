{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Facebook.Monad
  ( FacebookT
  , Auth
  , NoAuth
  , FbTier(..)
  , runFacebookT
  , runNoAuthFacebookT
  , beta_runFacebookT
  , beta_runNoAuthFacebookT
  , getApiVersion
  , getCreds
  , getMCreds
  , getManager
  , getTier
  , withTier
  , addAppSecretProof
  , makeAppSecretProof
  , runResourceInFb
  , mapFacebookT
  , setApiVersion
   -- * Re-export
  , lift
  ) where

import Control.Applicative (Alternative, Applicative)
import Control.Monad (MonadPlus, liftM)
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Fail (MonadFail(..))
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger(..))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Reader (ReaderT(..), ask, mapReaderT)
import qualified Control.Monad.Trans.Resource as R
import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC (HMAC(..), hmac)
import Data.ByteArray.Encoding (Base(..), convertToBase)
import qualified Data.Text.Encoding as TE
import Data.Typeable (Typeable)
import Facebook.Types
import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT
import UnliftIO
import qualified UnliftIO.Exception as E

-- | @FacebookT auth m a@ is this library's monad transformer.
-- Contains information needed to issue commands and queries to
-- Facebook.  The phantom type @auth@ may be either 'Auth' (you
-- have supplied your 'Credentials') or 'NoAuth' (you have not
-- supplied any 'Credentials').
newtype FacebookT auth m a = F
  { unF :: ReaderT FbData m a -- FbData -> m a
  } deriving ( Functor
             , Applicative
             , Alternative
             , Monad
             , MonadFix
             , MonadPlus
             , MonadIO
             , MonadTrans
             , R.MonadThrow
             , MonadFail
             )

instance (MonadUnliftIO m) => MonadUnliftIO (FacebookT auth m) where
  withRunInIO inner =
    F $
    ReaderT $ \r ->
      withRunInIO $ \run -> inner (\fbT -> run $ (flip runReaderT) r (unF fbT))

deriving instance
         (R.MonadResource m, MonadBase IO m) =>
         R.MonadResource (FacebookT auth m)

instance MonadBase b m => MonadBase b (FacebookT auth m) where
  liftBase = lift . liftBase

-- | Since @fb-0.14.8@.
instance MonadLogger m => MonadLogger (FacebookT auth m) where
  monadLoggerLog loc src lvl msg = lift (monadLoggerLog loc src lvl msg)

-- | Phantom type stating that you have provided your
-- 'Credentials' and thus have access to the whole API.
data Auth
  deriving (Typeable)

-- | Phantom type stating that you have /not/ provided your
-- 'Credentials'.  This means that you'll be limited about which
-- APIs you'll be able use.
data NoAuth
  deriving (Typeable)

-- | Internal data kept inside 'FacebookT'.
data FbData = FbData
  { fbdCreds :: Maybe Credentials
  , fbdManager :: !H.Manager
  , fbdTier :: !FbTier
  , fbdApiVersion :: IORef ApiVersion
  } deriving (Typeable)

-- | Which Facebook tier should be used (see
-- <https://developers.facebook.com/support/beta-tier/>).
data FbTier
  = Production
  | Beta
  deriving (Eq, Ord, Show, Read, Enum, Typeable)

defaultApiVersion :: ApiVersion
defaultApiVersion = "v3.2"

-- | Set the Graph API version.
setApiVersion :: (MonadIO m) => ApiVersion -> FacebookT anyAuth m ()
setApiVersion apiVersion = do
  ref <- fbdApiVersion `liftM` F ask
  atomicModifyIORef' ref (\_ -> (apiVersion, ()))
  return ()

-- | Run a computation in the 'FacebookT' monad transformer with
-- your credentials.
runFacebookT ::
     (MonadIO m)
  => Credentials -- ^ Your app's credentials.
  -> H.Manager -- ^ Connection manager (see 'H.withManager').
  -> FacebookT Auth m a
  -> m a
runFacebookT creds manager (F act) = do
  apiref <- newIORef defaultApiVersion
  runReaderT act (FbData (Just creds) manager Production apiref)

addAppSecretProof ::
     Credentials
  -> Maybe (AccessToken anykind)
  -> HT.SimpleQuery
  -> HT.SimpleQuery
addAppSecretProof (Credentials _ _ _ False) _ query = query
addAppSecretProof creds mtoken query = makeAppSecretProof creds mtoken <> query

-- | Make an appsecret_proof in case the given credentials access token is a
-- user access token.
-- See: https://developers.facebook.com/docs/graph-api/securing-requests/#appsecret_proof
makeAppSecretProof ::
     Credentials -- ^ App credentials
  -> Maybe (AccessToken anyKind) -- ^
  -> HT.SimpleQuery
makeAppSecretProof creds (Just (UserAccessToken _ accessToken _)) =
  [(TE.encodeUtf8 "appsecret_proof", proof)]
  where
    hmacData :: HMAC SHA256
    hmacData = hmac (appSecretBS creds) (TE.encodeUtf8 accessToken)
    proof = convertToBase Base16 hmacData
makeAppSecretProof _ _ = []

-- | Run a computation in the 'FacebookT' monad without
-- credentials.
runNoAuthFacebookT ::
     (MonadIO m)
  => H.Manager -- ^ Connection manager (see 'H.withManager').
  -> FacebookT NoAuth m a
  -> m a
runNoAuthFacebookT manager (F act) = do
  apiref <- newIORef defaultApiVersion
  runReaderT act (FbData Nothing manager Production apiref)

-- | Same as 'runFacebookT', but uses Facebook's beta tier (see
-- <https://developers.facebook.com/support/beta-tier/>).
beta_runFacebookT ::
     (MonadIO m) => Credentials -> H.Manager -> FacebookT Auth m a -> m a
beta_runFacebookT creds manager (F act) = do
  apiref <- newIORef defaultApiVersion
  runReaderT act (FbData (Just creds) manager Beta apiref)

-- | Same as 'runNoAuthFacebookT', but uses Facebook's beta tier
-- (see <https://developers.facebook.com/support/beta-tier/>).
beta_runNoAuthFacebookT ::
     (MonadIO m) => H.Manager -> FacebookT NoAuth m a -> m a
beta_runNoAuthFacebookT manager (F act) = do
  apiref <- newIORef defaultApiVersion
  runReaderT act (FbData Nothing manager Beta apiref)

-- | Get the user's credentials, fail if they are not available.
getCreds :: (Monad m, MonadIO m) => FacebookT Auth m Credentials
getCreds = do
  mCreds <- getMCreds
  case mCreds of
    Nothing -> E.throwIO $ FbLibraryException "Couldn't get credentials."
    Just creds -> return creds

-- | Get the user's credentials.
getMCreds :: Monad m => FacebookT anyAuth m (Maybe Credentials)
getMCreds = fbdCreds `liftM` F ask

-- | Get the Graph API version.
getApiVersion :: MonadIO m => FacebookT anyAuth m ApiVersion
getApiVersion = do
  ref <- fbdApiVersion `liftM` F ask
  apiVersion <- readIORef ref
  pure apiVersion

-- | Get the 'H.Manager'.
getManager :: Monad m => FacebookT anyAuth m H.Manager
getManager = fbdManager `liftM` F ask

-- | Get the 'FbTier'.
getTier :: Monad m => FacebookT anyAuth m FbTier
getTier = fbdTier `liftM` F ask

-- | Run a pure function that depends on the 'FbTier' being used.
withTier :: Monad m => (FbTier -> a) -> FacebookT anyAuth m a
withTier = flip liftM getTier

-- | Run a 'ResourceT' inside a 'FacebookT'.
runResourceInFb ::
     (R.MonadResource m, MonadUnliftIO m)
  => FacebookT anyAuth (R.ResourceT m) a
  -> FacebookT anyAuth m a
runResourceInFb (F inner) = F $ ask >>= lift . R.runResourceT . runReaderT inner

-- | Transform the computation inside a 'FacebookT'.
mapFacebookT :: (m a -> n b) -> FacebookT anyAuth m a -> FacebookT anyAuth n b
mapFacebookT f = F . mapReaderT f . unF
