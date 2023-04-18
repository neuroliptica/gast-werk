{-# LANGUAGE OverloadedStrings #-}

module Logger
  ( logBoth
  , writeInBuffer
  ) where

import Control.Logging as Log
import Data.Text as T
import GI.Gtk
import Data.ByteString.Char8 as B8
import Data.Int

-- | Insert Text in TextBuffer at TextIter position.
textBufferInsertByteString :: TextBuffer -> TextIter -> T.Text -> IO ()
textBufferInsertByteString buf pos msg = do
    let len = fromIntegral (B8.length . B8.pack . T.unpack $ msg) :: Int32
    textBufferInsert buf pos msg len

-- | Write Text in Gtk.TextBuffer with MaxLines param specified.
writeInBuffer :: TextBuffer -> T.Text -> Int32 -> IO ()
writeInBuffer buffer msg maxlines = do
    end <- textBufferGetEndIter buffer
    textBufferInsertByteString buffer end msg
    textBufferInsertByteString buffer end "\n"
    len <- textBufferGetLineCount buffer
    if (len > maxlines)
    then do
      start <- textBufferGetStartIter buffer
      until <- textBufferGetIterAtLine buffer (len - maxlines)
      textBufferDelete buffer start until
    else return ()

-- | Write message both in buffer and stdout.
logBoth :: TextBuffer -> T.Text -> IO ()
logBoth buf msg = Log.withStdoutLogging $ do
    Log.log msg
    writeInBuffer buf msg 10
