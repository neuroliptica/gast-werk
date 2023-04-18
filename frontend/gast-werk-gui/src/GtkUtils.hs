{-# LANGUAGE OverloadedStrings #-}

module GtkUtils
  ( createDefaultLogBuffer
  ) where

import GI.Gtk
import Data.Text as T (pack)
import Logger
import Data.Int (Int32)

-- | Create TextBuffer with N+1 empty lines.
createDefaultLogBuffer :: Int -> IO TextBuffer
createDefaultLogBuffer maxLines = do
    logBuffer <- (textBufferNew :: Maybe TextTagTable -> IO TextBuffer) Nothing
    let start = T.pack . take (maxLines + 1) $ repeat '\n'
    writeInBuffer logBuffer start (fromIntegral maxLines :: Int32)
    return logBuffer

