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
{-# LANGUAGE RankNTypes #-}
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
  , getManager
  , getTier
  , withTier
  , getAppSecretProofAdder
  , runResourceInFb
  , mapFacebookT
   -- * Re-export
  , lift
  ) where

import Control.Applicative (Applicative, Alternative)
import Control.Monad (MonadPlus, liftM)
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Fail
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger(..))
import Control.Monad.Trans.Class (MonadTrans(lift))
import UnliftIO
import Control.Monad.Trans.Reader (ReaderT(..), ask, mapReaderT)
import Data.Typeable (Typeable)
import qualified Control.Monad.Trans.Resource as R

import Crypto.Hash.CryptoAPI (SHA256)
import Crypto.HMAC (hmac', MacKey(..))
import qualified Data.Serialize as Cereal
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
  } deriving (Functor, Applicative, Alternative, Monad, MonadFix, MonadFail, MonadPlus, MonadIO, MonadTrans, R.MonadThrow)

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
  { fbdCreds :: Credentials -- ^ Can be 'undefined'!
  , fbdManager :: !H.Manager
  , fbdTier :: !FbTier
  , fbdApiVersion :: !ApiVersion
  , fbdAppSecretProofAdder :: forall anyKind. Maybe (AccessToken anyKind)
                           -> HT.SimpleQuery
                           -> HT.SimpleQuery
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
  runReaderT act (FbData creds manager Production apiVersion  $ addAppSecretProof creds )


addAppSecretProof :: Credentials
                  -> Maybe (AccessToken anykind)
                  -> HT.SimpleQuery
                  -> HT.SimpleQuery
addAppSecretProof creds mtoken query = makeAppSecretProof creds mtoken <> query

-- | Make an appsecret_proof in case the given credentials access token is a
-- user access token.
-- See: https://developers.facebook.com/docs/graph-api/securing-requests/#appsecret_proof
makeAppSecretProof
  :: Credentials         -- ^ App credentials
  -> Maybe ( AccessToken anyKind ) -- ^
  -> HT.SimpleQuery
makeAppSecretProof creds (Just ( UserAccessToken _ accessToken _ ))
  = [( TE.encodeUtf8 "appsecret_proof", proof)]

  where
    key :: MacKey ctx SHA256
    key   = MacKey $ appSecretBS creds
    proof = Cereal.encode $ hmac' key ( TE.encodeUtf8 accessToken )
makeAppSecretProof  _ _ = []


-- | Run a computation in the 'FacebookT' monad without
-- credentials.
runNoAuthFacebookT
  :: ApiVersion -- ^ Api version
  -> H.Manager -- ^ Connection manager (see 'H.withManager').
  -> FacebookT NoAuth m a -> m a
runNoAuthFacebookT apiVersion manager (F act) =
  let creds = error "runNoAuthFacebookT: never here, serious bug"
  in runReaderT act (FbData creds manager Production apiVersion $ const id)
 

-- | Same as 'runFacebookT', but uses Facebook's beta tier (see
-- <https://developers.facebook.com/support/beta-tier/>).
beta_runFacebookT :: Credentials -> ApiVersion -> H.Manager -> FacebookT Auth m a -> m a
beta_runFacebookT creds apiVersion manager (F act) =
  runReaderT act (FbData creds manager Beta apiVersion $ addAppSecretProof creds)

-- | Same as 'runNoAuthFacebookT', but uses Facebook's beta tier
-- (see <https://developers.facebook.com/support/beta-tier/>).
beta_runNoAuthFacebookT :: ApiVersion -> H.Manager -> FacebookT NoAuth m a -> m a
beta_runNoAuthFacebookT apiVersion manager (F act) =
  let creds = error "beta_runNoAuthFacebookT: never here, serious bug"
  in runReaderT act (FbData creds manager Beta apiVersion $ const id)

-- | Get the user's credentials.
getCreds
  :: Monad m
  => FacebookT Auth m Credentials
getCreds = fbdCreds `liftM` F ask

-- | Get the Graph API version.
getApiVersion
  :: Monad m
  => FacebookT anyAuth m ApiVersion
getApiVersion = fbdApiVersion `liftM` F ask

getAppSecretProofAdder
  :: Monad m
  => FacebookT anyAuth m ( Maybe (AccessToken anyKind)
                        -> HT.SimpleQuery
                        -> HT.SimpleQuery )
getAppSecretProofAdder = fbdAppSecretProofAdder `liftM` F ask

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
