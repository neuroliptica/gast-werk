{-# LANGUAGE OverloadedStrings #-}

-- | Json.hs: provide json schemas.
module Json
  ( ApiGet(..)
  ) where

import Data.Aeson

-- | /api/get response.
data ApiGet = ApiGet String String Int
    deriving Show

instance FromJSON ApiGet where
    parseJSON (Object v) = do
        base64 <- v .: "img_base64"
        hash <- v .: "hash"
        empty <- v .: "empty"
        return (ApiGet base64 hash empty)

