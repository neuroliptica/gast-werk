{-# LANGUAGE OverloadedStrings #-}

-- | Json.hs: provide json schemas.
module Json
  ( GetResponse(..)
  , Error(..)
  ) where

import Data.Aeson hiding (Error)

-- | General error body scheme.
data ErrorBody = ErrorBody Bool String
    deriving Show

instance FromJSON ErrorBody where
    parseJSON (Object v) = do
        failed <- v .: "failed"
        reason <- v .: "reason"
        return (ErrorBody failed reason)

-- | General error scheme.
data Error = Error ErrorBody
    deriving Show

instance FromJSON Error where
    parseJSON (Object v) = do
        body <- v .: "error"
        return (Error body)

-- | /api/get response scheme.
data GetResponse = GetResponse String String Int
    deriving Show

instance FromJSON GetResponse where
    parseJSON (Object v) = do
        base64 <- v .: "img_base64"
        hash <- v .: "hash"
        empty <- v .: "empty"
        return (GetResponse base64 hash empty)

