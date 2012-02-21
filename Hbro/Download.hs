module Hbro.Download where

-- {{{ Imports
import Hbro.Types
import Hbro.Util

import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Display.Label

import Network.URI

import System.FilePath
-- }}}

labelNotify :: Environment -> IO ()
labelNotify env = do
    feedbackLabel <- builderGetObject ((mBuilder . mGUI) env)  castToLabel "feedback"
    labelSetMarkupTemporary feedbackLabel "<span foreground=\"green\">Download started</span>" 5000
  

aria, wget, axel :: PortableFilePath -> URI -> String -> IO ()
aria path' uri filename = resolve path' >>= \destination -> spawn "aria2c" [show uri, "-d", destination, "-o", filename]
wget path' uri filename = resolve path' >>= \destination -> spawn "wget"   [show uri, "-O", destination </> filename]
axel path' uri filename = resolve path' >>= \destination -> spawn "axel"   [show uri, "-o", destination </> filename]
