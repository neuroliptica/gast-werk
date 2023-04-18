{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Requests.hs: provide geenral api for interacting through requests.
module Requests 
  ( jsonPost
  , jsonGet
  , apiGet
  ) where

import Data.Aeson hiding (Error)
import Network.HTTP.Simple
import Control.Logging as Log
import Data.Text as T
import Data.ByteString.Lazy as LBS

import Json

-- | Server body answer. Could be either a general error or a valid answer.
type Answer l r = Maybe (Either l r)
-- | Answer but inside IO monad.
type AnswerIO l r = IO (Answer l r)

-- | Generic decoder for api answers. Will return Nothing if both answer schemas failed.
decoder :: (FromJSON l, FromJSON r) => LBS.ByteString -> Answer l r
decoder answer = do
    case decode answer :: FromJSON r => Maybe r of
      Just valid -> return $ Right valid
      Nothing -> do
        failed <- decode answer :: FromJSON l => Maybe l
        return $ Left failed

-- | General HTTP POST with json request and response body.
jsonPost :: ToJSON req => String -> req -> IO LBS.ByteString
jsonPost url body = Log.withStdoutLogging $ do
    Log.logS "POST" $ T.pack url
    request <- setRequestBodyJSON body <$> parseRequest ("POST " <> url)
    response <- httpLBS request
    return $ getResponseBody response

-- | General HTTP GET with json response body.
jsonGet :: String -> IO LBS.ByteString
jsonGet url = Log.withStdoutLogging $ do
    Log.logS "GET" $ T.pack url
    response <- httpLBS <$> parseRequest url
    getResponseBody <$> response

-- | HTTP GET to /api/get
apiGet :: AnswerIO Error GetResponse
apiGet = do
    response <- jsonGet "http://localhost:8080/api/get"
    return (decoder response :: Answer Error GetResponse)

