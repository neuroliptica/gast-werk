{-# LANGUAGE OverloadedStrings #-}

-- | Cache.hs: manipulating with cache files.
module Cache
  ( createCacheFile
  , readCacheFile 
  ) where

import Control.Logging as Log
import System.Directory
import Data.Text as T

-- | If file do not exist, then create it with empty content.
createCacheFile :: String -> IO ()
createCacheFile name = Log.withStdoutLogging $ do
    Log.log "checking cache file..."
    exist <- doesFileExist name
    if not exist
    then do
      Log.log "cache file doesn't exist, creating..."
      writeFile name ""
      Log.log $ T.pack ("done => " <> name)
    else return ()

-- | Read file content, may fail.
readCacheFile :: String -> IO (Maybe String)
readCacheFile name = Log.withStdoutLogging $ do
    Log.log $ T.pack ("reading cache file... " <> name)
    exist <- doesFileExist name
    if not exist
    then do
      Log.log "failed, cache file doesn't exist"
      return Nothing
    else Just <$> readFile name
