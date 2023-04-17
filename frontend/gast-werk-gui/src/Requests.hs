{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Requests.hs: provide geenral api for interacting through requests.
module Requests 
  ( jsonPost
  , jsonGet
  ) where

import Data.Aeson
import Network.HTTP.Simple
import Control.Logging as Log
import Data.Text as T
import Data.ByteString.Lazy as LBS

-- | General HTTP POST with json request and response body.
jsonPost :: ToJSON req => String -> req -> IO LBS.ByteString
jsonPost url body = Log.withStdoutLogging $ do
    Log.logS "POST" $ T.pack url
    request <- setRequestBodyJSON body <$> parseRequest ("POST " <> url)
    response <- httpLBS request
    return $ getResponseBody response

-- | General HTTP GET with json response body.
jsonGet :: String -> IO LBS.ByteString
jsonGet url = do
    Log.logS "GET" $ T.pack url
    response <- httpLBS <$> parseRequest url
    getResponseBody <$> response

