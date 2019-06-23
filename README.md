fb
--

[![Build Status](https://travis-ci.org/psibi/fb.svg?branch=master)](https://travis-ci.org/psibi/fb)

Haskell bindings to Facebook's API


## Example code to get User Access token

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Facebook
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Data.Monoid ((<>))
import Data.ByteString.Char8 (pack)
import Data.Text hiding (pack)
import Data.Aeson
import qualified Data.Text.Encoding as TE

myCreds :: Credentials
myCreds =
  Credentials
  { appName = "Your_APP_Name"
  , appId = "your_app_id"
  , appSecret = "xxxxxxxxxxxxxxxxx"
  , appSecretProof = False
  }

main :: IO ()
main = do
  mgr <- newManager tlsManagerSettings
  let redirectUrl = "https://www.yourdomain.com/"
  runResourceT $
    runFacebookT myCreds mgr $
    do url1 <- getUserAccessTokenStep1 redirectUrl ["public_profile", "email"]
       liftIO $ print ("Paste the url in browser and get code: " <> url1)
       code <- liftIO $ getLine
       token <- getUserAccessTokenStep2 redirectUrl [("code", pack code)]
       liftIO $ print token
```

## Snippet to get your Profile Picture:

```
       (picture :: Value) <-
         getObject "/me/picture" [("redirect", "0")] (Just token)
       liftIO $ print picture
```

## Snippet to get your firstname, lastname:

```
       user <- getUser "me" [("fields", "first_name,last_name")] (Just token)
       liftIO $ print user
```
