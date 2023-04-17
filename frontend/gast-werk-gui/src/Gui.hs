{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
-- {-# LANGUAGE NoMonomorphismRestriction          #-}

-- | Gui.hs: main gui instance.
module Gui
  ( gui
  ) where

import GI.Gtk hiding (main, init)
import GI.Gtk as Gtk (main, init)

import Control.Logging as Log
import Logger

gui :: IO ()
gui = do
    Gtk.init Nothing
    mainWindow <- windowNew WindowTypeToplevel
    windowSetTitle       mainWindow "gast-werk gui"
    windowSetDefaultSize mainWindow 700 700
    windowSetPosition    mainWindow WindowPositionCenter
    onWidgetDestroy      mainWindow mainQuit

    vPaned <- panedNew OrientationVertical
    containerAdd mainWindow vPaned

    topFrame <- frameNew Nothing
    topFrameBox <- boxNew OrientationVertical 1

    panedPack1 vPaned topFrame False False
    containerAdd topFrame topFrameBox

    button <- buttonNewWithLabel "Testing"
    containerAdd topFrameBox button


    logFrame <- frameNew Nothing
    logFrameBox <- boxNew OrientationVertical 1

    logBuffer <- (textBufferNew :: Maybe TextTagTable -> IO TextBuffer) Nothing
    logView <- textViewNewWithBuffer logBuffer

    onButtonClicked button $ do
        logBoth logBuffer "button clicked"

    panedPack2 vPaned logFrame False False
    containerAdd logFrame logFrameBox
    containerAdd logFrameBox logView

    widgetShowAll mainWindow
    Gtk.main
