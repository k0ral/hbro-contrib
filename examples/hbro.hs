{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
module Main where

-- {{{ Imports
import           Hbro
import qualified Hbro.Bookmarks                     as Bookmarks
import qualified Hbro.Clipboard                     as Clipboard
import           Hbro.Config                        (homePageL)
import qualified Hbro.Config                        as Config
import qualified Hbro.Download                      as Download
import qualified Hbro.Gui                           as GUI
import           Hbro.Gui.PromptBar
import           Hbro.Gui.PromptBar.Signals
import qualified Hbro.History                       as History
import           Hbro.Keys                          as Key
import           Hbro.Keys.Model                    ((.|))
import           Hbro.Keys.Monadic                  as Key
import           Hbro.Misc
import           Hbro.Settings
import           Hbro.StatusBar
import qualified Hbro.Webkit.WebSettings            as WebSettings
import           Hbro.WebView.Hooks                 as WebView
import           Hbro.WebView.Signals

import qualified Data.Set                           as Set

import           Filesystem

import           Graphics.UI.Gtk.Display.Label
import           Graphics.UI.Gtk.WebKit.WebSettings

import qualified Network.URI                        as N
import           Network.URI.Monadic
-- }}}


myHomePage = fromJust . N.parseURI $ "http://www.google.com"

-- Download to $HOME
myDownloadHook :: (BaseIO m) => Download -> m ()
myDownloadHook (Download uri filename _size) = do
    destination <- io getHomeDirectory
    Download.aria destination uri filename

-- myLoadFinishedHook :: KE ()
myLoadFinishedHook _ = History.log

-- Setup (run at start-up)
-- Note that keybindings are suited for an azerty keyboard
mySetup :: K ()
mySetup = do
    Config.set  homePageL       myHomePage
    WebView.set onDownloadL     myDownloadHook
    WebView.set onLoadFinishedL myLoadFinishedHook

-- Browse
    Key.bind (_Control .| _Left)  $  goBackList    ["-l", "10"] >>= load
    Key.bind (_Control .| _Right) $  goForwardList ["-l", "10"] >>= load
    Key.bind (_Control .| _g)     $  prompt "DuckDuckGo search" "" >>= parseURIReference . ("http://duckduckgo.com/html?q=" ++) . (pack . escapeURIString isAllowedInURI . unpack) >>= load
-- Bookmarks
    Key.bind (_Control .| _d)      $     prompt "Bookmark with tags:" "" >>= Bookmarks.add . Set.fromList . words
{-    Key.bind (_Control .| _D)      $     Prompt.read "Bookmark all instances with tag:" "" $ \tags -> do
        uris <- mapM parseURI =<< sendCommandToAll "GET_URI"
        forM uris $ Bookmarks.addCustom . (`Bookmarks.Entry` words tags)
        void . Bookmarks.addCustom . (`Bookmarks.Entry` words tags) =<< getURI-}
    Key.bind (_Alt .| _d)          $     Bookmarks.deleteByTag ["-l", "10"]
    Key.bind (_Control .| _l)      $     Bookmarks.select      ["-l", "10"] >>= load
    Key.bind (_Control .| _L)      $     Bookmarks.selectByTag ["-l", "10"] >>= void . mapM (\uri -> io $ spawn "hbro" ["-u", show uri])
-- History
    Key.bind (_Alt .| _h)          $     load . History._uri =<< History.select ["-l", "10"]
-- Settings
    Key.bind (_Alt .| _j)          $     WebSettings.toggle_ webSettingsEnableScripts
    Key.bind (_Alt .| _p)          $     WebSettings.toggle_ webSettingsEnablePlugins

-- Web settings (cf Graphic.Gtk.WebKit.WebSettings)
    WebSettings.set webSettingsEnablePlugins       False
    WebSettings.set webSettingsEnableScripts       True
    WebSettings.set webSettingsJSCanOpenWindowAuto True
    WebSettings.set webSettingsUserAgent           firefoxUserAgent

-- Status bar customization: scroll position + zoom level + load progress + current URI + key strokes
    installScrollWidget =<< GUI.getObject castToLabel "scroll"
    installZoomWidget   =<< GUI.getObject castToLabel "zoom"
    installProgressWidget =<< GUI.getObject castToLabel "progress"
    installURIWidget defaultURIColors defaultSecureURIColors =<< GUI.getObject castToLabel "uri"
    installKeyStrokesWidget =<< GUI.getObject castToLabel "keys"

    return ()


-- Main function, expected to call 'hbro'
main :: IO ()
main = hbro mySetup
