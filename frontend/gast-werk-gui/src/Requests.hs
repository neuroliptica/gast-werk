{-# LANGUAGE OverloadedStrings #-}

-- | Requests.hs: provide geenral api for interacting through requests.
module Requests 
  ( jsonPost
  , jsonGet
  ) where

import Data.Aeson
import Network.HTTP.Simple

-- | General HTTP POST with json request and response body.
jsonPost :: (ToJSON req, FromJSON resp) => String -> req -> IO resp
jsonPost url body = do
    initReq <- parseRequest ("POST " <> url)
    let request = setRequestBodyJSON body initReq
    response <- httpJSON request :: (FromJSON resp) => IO (Response resp)
    return $ getResponseBody response

-- | General HTTP GET with json response body.
jsonGet :: (FromJSON resp) => String -> IO resp
jsonGet url = do
    initReq <- parseRequest url
    response <- httpJSON initReq :: (FromJSON resp) => IO (Response resp)
    return $ getResponseBody response


