{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
   -- * Re-export
  , lift
  ) where

import Control.Applicative (Applicative, Alternative)
import Control.Monad (MonadPlus, liftM)
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger(..))
import Control.Monad.Trans.Class (MonadTrans(lift))
import UnliftIO
import Control.Monad.Trans.Reader (ReaderT(..), ask, mapReaderT)
import qualified Control.Monad.Trans.Resource as R
import qualified UnliftIO.Exception as E
import Data.Typeable (Typeable)
import qualified Data.ByteString.Base16 as Base16

import Crypto.Hash.CryptoAPI (SHA256)
import Crypto.HMAC (hmac', MacKey(..))
import Crypto.Classes (encode)
import qualified Data.Text.Encoding as TE

import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT

import Facebook.Types

-- | @FacebookT auth m a@ is this library's monad transformer.
-- Contains information needed to issue commands and queries to
-- Facebook.  The phantom type @auth@ may be either 'Auth' (you
-- have supplied your 'Credentials') or 'NoAuth' (you have not
-- supplied any 'Credentials').
newtype FacebookT auth m a = F
  { unF :: ReaderT FbData m a -- FbData -> m a
  } deriving (Functor, Applicative, Alternative, Monad, MonadFix, MonadPlus, MonadIO, MonadTrans, R.MonadThrow)

deriving instance
         (R.MonadResource m, MonadBase IO m) =>
         R.MonadResource (FacebookT auth m)

instance MonadBase b m =>
         MonadBase b (FacebookT auth m) where
  liftBase = lift . liftBase

-- askUnliftIO = ReaderT $ \r ->
--                 withUnliftIO $ \u ->
-- return (UnliftIO (unliftIO u . flip runReaderT r))
instance (MonadIO m, MonadUnliftIO m) =>
         MonadUnliftIO (FacebookT auth m) where
  askUnliftIO :: FacebookT auth m (UnliftIO (FacebookT auth m))
  askUnliftIO =
    F
      (ReaderT $
       \(r :: FbData) ->
          withUnliftIO $
          \(u :: UnliftIO m) ->
             return
               (UnliftIO
                  (\(x :: FacebookT auth m a) ->
                      (unliftIO u ((flip runReaderT r) (unF x))))))

-- | Since @fb-0.14.8@.
instance MonadLogger m =>
         MonadLogger (FacebookT auth m) where
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
  , fbdApiVersion :: !ApiVersion
  } deriving (Typeable)

-- | Which Facebook tier should be used (see
-- <https://developers.facebook.com/support/beta-tier/>).
data FbTier
  = Production
  | Beta
  deriving (Eq, Ord, Show, Read, Enum, Typeable)

-- | Run a computation in the 'FacebookT' monad transformer with
-- your credentials.
runFacebookT
  :: Credentials -- ^ Your app's credentials.
  -> ApiVersion -- ^ Api version
  -> H.Manager -- ^ Connection manager (see 'H.withManager').
  -> FacebookT Auth m a
  -> m a
runFacebookT creds apiVersion manager (F act) =
  runReaderT act (FbData ( Just creds ) manager Production apiVersion)


addAppSecretProof :: Credentials
                  -> Maybe (AccessToken anykind)
                  -> HT.SimpleQuery
                  -> HT.SimpleQuery
addAppSecretProof ( Credentials _ _ _ False ) _ query = query
addAppSecretProof creds mtoken query = makeAppSecretProof creds mtoken <> query

-- | Make an appsecret_proof in case the given credentials access token is a
-- user access token.
-- See: https://developers.facebook.com/docs/graph-api/securing-requests/#appsecret_proof
makeAppSecretProof
  :: Credentials -- ^ App credentials
  -> Maybe ( AccessToken anyKind ) -- ^
  -> HT.SimpleQuery
makeAppSecretProof creds (Just ( UserAccessToken _ accessToken _ ))
  = [( TE.encodeUtf8 "appsecret_proof", proof)]

  where
    key :: MacKey ctx SHA256
    key   = MacKey $ appSecretBS creds
    proof = Base16.encode $ encode $ hmac' key ( TE.encodeUtf8 accessToken )
makeAppSecretProof  _ _ = []


-- | Run a computation in the 'FacebookT' monad without
-- credentials.
runNoAuthFacebookT
  :: ApiVersion -- ^ Api version
  -> H.Manager -- ^ Connection manager (see 'H.withManager').
  -> FacebookT NoAuth m a -> m a
runNoAuthFacebookT apiVersion manager (F act) =
  runReaderT act (FbData Nothing manager Production apiVersion)


-- | Same as 'runFacebookT', but uses Facebook's beta tier (see
-- <https://developers.facebook.com/support/beta-tier/>).
beta_runFacebookT :: Credentials -> ApiVersion -> H.Manager -> FacebookT Auth m a -> m a
beta_runFacebookT creds apiVersion manager (F act) =
  runReaderT act (FbData (Just creds) manager Beta apiVersion)

-- | Same as 'runNoAuthFacebookT', but uses Facebook's beta tier
-- (see <https://developers.facebook.com/support/beta-tier/>).
beta_runNoAuthFacebookT :: ApiVersion -> H.Manager -> FacebookT NoAuth m a -> m a
beta_runNoAuthFacebookT apiVersion manager (F act) =
  runReaderT act (FbData Nothing manager Beta apiVersion)

-- | Get the user's credentials, fail if they are not available.
getCreds
  :: (Monad m, MonadIO m)
  => FacebookT Auth m Credentials
getCreds = do
   mCreds <- getMCreds
   case mCreds of
     Nothing -> E.throwIO $ FbLibraryException "Couldn't get credentials."
     Just creds -> return creds

-- | Get the user's credentials.
getMCreds
  :: Monad m
  => FacebookT anyAuth m (Maybe Credentials)
getMCreds = fbdCreds `liftM` F ask

-- | Get the Graph API version.
getApiVersion
  :: Monad m
  => FacebookT anyAuth m ApiVersion
getApiVersion = fbdApiVersion `liftM` F ask

-- | Get the 'H.Manager'.
getManager
  :: Monad m
  => FacebookT anyAuth m H.Manager
getManager = fbdManager `liftM` F ask

-- | Get the 'FbTier'.
getTier
  :: Monad m
  => FacebookT anyAuth m FbTier
getTier = fbdTier `liftM` F ask

-- | Run a pure function that depends on the 'FbTier' being used.
withTier
  :: Monad m
  => (FbTier -> a) -> FacebookT anyAuth m a
withTier = flip liftM getTier

-- | Run a 'ResourceT' inside a 'FacebookT'.
runResourceInFb
  :: (R.MonadResource m, MonadUnliftIO m)
  => FacebookT anyAuth (R.ResourceT m) a -> FacebookT anyAuth m a
runResourceInFb (F inner) = F $ ask >>= lift . R.runResourceT . runReaderT inner

-- | Transform the computation inside a 'FacebookT'.
mapFacebookT :: (m a -> n b) -> FacebookT anyAuth m a -> FacebookT anyAuth n b
mapFacebookT f = F . mapReaderT f . unF
