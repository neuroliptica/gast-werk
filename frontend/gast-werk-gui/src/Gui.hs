{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction          #-}

-- | Gui.hs: main gui instance.
module Gui
  ( gui
  ) where

import GI.Gtk hiding (main, init)
import GI.Gtk as Gtk (main, init)

gui :: IO ()
gui = do
    Gtk.init Nothing
    mainWindow <- windowNew WindowTypeToplevel
    windowSetTitle       mainWindow "gast-werk gui"
    windowSetDefaultSize mainWindow 500 500
    windowSetPosition    mainWindow WindowPositionCenter
    onWidgetDestroy      mainWindow mainQuit

    widgetShowAll mainWindow
    Gtk.main
