{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

-- {{{ Imports
import           Hbro
import           Hbro.Attributes
import qualified Hbro.Bookmarks                     as Bookmarks
import qualified Hbro.Clipboard                     as Clipboard
import           Hbro.Config                        (homePageL)
import qualified Hbro.Config                        as Config
import           Hbro.Defaults
import qualified Hbro.Download                      as Download
import           Hbro.Gui.PromptBar
import qualified Hbro.History                       as History
import           Hbro.Keys                          as Key
import           Hbro.Keys.Model                    ((.|))
import           Hbro.Misc
import           Hbro.Settings
import           Hbro.StatusBar

import           Control.Lens.Getter

import qualified Data.Map                           as Map
import qualified Data.Set                           as Set

import           Filesystem

import           Graphics.UI.Gtk.WebKit.WebSettings

import qualified Network.URI                        as N
import           Network.URI.Monadic
-- }}}


myHomePage = fromJust . N.parseURI $ "https://www.google.com"

-- Download to $HOME
myDownloadHook :: (ControlIO m) => (URI, Text, Maybe Int) -> m ()
myDownloadHook (uri, filename, _size) = do
    destination <- io getHomeDirectory
    Download.aria destination uri filename

myLoadFinishedHook :: (ControlIO m, MainViewReader m, MonadError Text m) => m ()
myLoadFinishedHook = History.log

-- Those key bindings are suited for an azerty keyboard
myKeyMap :: (OmniReader m) => KeyMap m
myKeyMap = defaultKeyMap <> Map.fromList
  -- Browse
    [ [_Control .| _Left]  >:  goBackList    ["-l", "10"] >>= load
    , [_Control .| _Right] >:  goForwardList ["-l", "10"] >>= load
    , [_Control .| _g]     >:  promptM "DuckDuckGo search" "" >>= parseURIReference . ("http://duckduckgo.com/html?q=" ++) . pack . escapeURIString isAllowedInURI . unpack >>= load
-- Bookmarks
    , [_Control .| _d]     >:  promptM "Bookmark with tags:" "" >>= Bookmarks.add . words
    -- , [_Control .| _D]     >:  promptM "Bookmark all instances with tag:" "" >>= \tags -> do
    --     uris <- mapM parseURI =<< sendCommandToAll "GET_URI"
    --     forM uris $ Bookmarks.addCustom . (`Bookmarks.Entry` words tags)
    --     void . Bookmarks.addCustom . (`Bookmarks.Entry` words tags) =<< getURI
    , [_Alt .| _d]         >:  Bookmarks.deleteByTag ["-l", "10"]
    , [_Control .| _l]     >:  Bookmarks.select      ["-l", "10"] >>= load
    , [_Control .| _L]     >:  Bookmarks.selectByTag ["-l", "10"] >>= void . mapM (\uri -> io $ spawn "hbro" ["-u", show uri])
-- History
    , [_Alt .| _h]         >:  load . History._uri =<< History.select ["-l", "10"]
-- Settings
    , [_Alt .| _j]         >:  getWebSettings >>= \s -> toggle_ s webSettingsEnableScripts
    , [_Alt .| _p]         >:  getWebSettings >>= \s -> toggle_ s webSettingsEnablePlugins
    ]


-- Setup run at start-up
myStartUpHook :: (OmniReader m) => m ()
myStartUpHook = do
    Config.set homePageL myHomePage

    mainView <- getMainView
    addHook (mainView^.downloadHookL) myDownloadHook
    addHook (mainView^.loadFinishedHookL) $ const myLoadFinishedHook

-- Web settings (cf Graphic.Gtk.WebKit.WebSettings)
    s <- getWebSettings
    set s webSettingsEnablePlugins       False
    set s webSettingsEnableScripts       True
    set s webSettingsJSCanOpenWindowAuto True
    set s webSettingsUserAgent           firefoxUserAgent

-- Status bar customization: scroll position + zoom level + load progress + current URI + key strokes
    b <- getBuilder
    installScrollWidget =<< getWidget b "scroll"
    installZoomWidget =<< getWidget b "zoom"
    installProgressWidget =<< getWidget b "progress"
    installURIWidget defaultURIColors defaultSecureURIColors =<< getWidget b "uri"
    installKeyStrokesWidget =<< getWidget b "keys"

    return ()


-- Main function, expected to call 'hbro'
main :: IO ()
main = hbro $ def { keyMap = myKeyMap, startUpHook = myStartUpHook }
