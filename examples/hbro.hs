{-# LANGUAGE FlexibleContexts #-}
module Main where

-- {{{ Imports
import Hbro
import qualified Hbro.Bookmarks as Bookmarks
import qualified Hbro.Clipboard as Clipboard
import qualified Hbro.Download as Download
import qualified Hbro.History as History
import Hbro.Misc
import qualified Hbro.Prompt as Prompt
import Hbro.Session
import Hbro.Settings
import Hbro.StatusBar
import qualified Hbro.Webkit.WebSettings as WS

import Control.Conditional
import Control.Monad hiding(forM_, mapM_)
import Control.Monad.Error hiding(forM_, mapM_)
import Control.Monad.IO.Class

import Data.Default
import Data.Foldable
import Data.Functor
import Data.Monoid
import Data.Time

import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Display.Label
import Graphics.UI.Gtk.Entry.Entry
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Gdk.GC
import Graphics.UI.Gtk.General.General
import Graphics.UI.Gtk.WebKit.Download
import Graphics.UI.Gtk.WebKit.NetworkRequest
import Graphics.UI.Gtk.WebKit.WebNavigationAction
import Graphics.UI.Gtk.WebKit.WebSettings
import Graphics.UI.Gtk.Windows.Window

import Network.URI hiding(parseURI, parseURIReference)

import Prelude hiding(mapM_)

import System.Directory
import System.Environment
import System.Environment.XDG.BaseDir
import System.FilePath
import System.Glib.Attributes
import System.Glib.Signals
-- import System.Posix.Process
import System.Process
-- }}}

-- {{{ Configuration structures
-- Any field you don't override will use the defaults defined in Hbro.Types.Config.
myConfig :: Config
myConfig = def {
    __UIFile           = myUIFile,
    __homePage         = myHomePage}
-- }}}

-- {{{ Constant parameters
myHomePage = URI "https:" (Just $ URIAuth "" "//duckduckgo.com" "") "" "" ""

myUIFile, myHistoryFile, myBookmarksFile, myDownloadDirectory :: IO FilePath
myUIFile            = getUserConfigDir "hbro" >/> "ui.xml"
myHistoryFile       = getUserDataDir   "hbro" >/> "history"
myBookmarksFile     = getUserDataDir   "hbro" >/> "bookmarks"
myDownloadDirectory = getHomeDirectory
-- }}}

-- {{{ Keys
-- Note that this example is suited for an azerty keyboard.
myKeys :: KeysList
myKeys = def <> KeysList [
-- Browse
    ("C-<Left>",      goBackList    ["-l", "10"] >>= loadURI),
    ("C-<Right>",     goForwardList ["-l", "10"] >>= loadURI),
    ("C-g",           Prompt.read "DuckDuckGo search" "" (loadURI <=< parseURIReference . ("https://duckduckgo.com/html?q=" ++) . escapeURIString isAllowedInURI)),
-- Bookmarks
    ("C-d",           Prompt.read "Bookmark with tags:" "" $ Bookmarks.add myBookmarksFile . words),
    ("C-D",           Prompt.read "Bookmark all instances with tag:" "" $ \tags -> do
        uris <- mapM parseURI =<< sendCommandToAll "GET_URI"
        forM uris $ Bookmarks.addCustom myBookmarksFile . (`Bookmarks.Entry` words tags)
        void . Bookmarks.addCustom myBookmarksFile . (`Bookmarks.Entry` words tags) =<< getURI
    ),
    ("M-d",           Bookmarks.deleteWithTag myBookmarksFile ["-l", "10"]),
    ("C-l",           Bookmarks.select        myBookmarksFile ["-l", "10"] >>= loadURI),
    ("C-L",           Bookmarks.selectTag     myBookmarksFile ["-l", "10"] >>= void . mapM (\uri -> io $ spawn "hbro" ["-u", (show uri)])),
--    ("C-q"),           webViewGetUri webView >>= maybe (return ()) (Queue.append),
--    ("M-q"),           \b -> do
--        uri <- Queue.popFront
--        loadURI uri b),

-- History
    ("C-h",           History.select myHistoryFile ["-l", "10"] >>= loadURI . History.mURI),

-- Session
    --("M-l"),           loadFromSession ["-l", "10"])
-- Settings
    ("M-j",           WS.toggle webSettingsEnableScripts >>= ((notify 5000 "Javascript disabled") ?? (notify 5000 "Javascript enabled"))),
    ("M-p",           WS.toggle webSettingsEnablePlugins >>= ((notify 5000 "Plugins disabled") ?? (notify 5000 "Plugins enabled")))
    ]
-- }}}

myDownloadHook :: DownloadHook
myDownloadHook = DownloadHook $ \uri filename _size -> Download.aria myDownloadDirectory uri filename

myKeyHook :: KeyHook
myKeyHook = emacsKeyHandler myKeys ["M-x"]-- >=> void . (printInLabel "keys")

myLoadFinishedHook = LoadFinishedHook $ History.log myHistoryFile

-- Main function, expected to call launchHbro.
main :: IO ()
main = hbro myConfig $ Setup $ do
-- Hooks
    afterKeyPressed     myKeyHook
    onDownload          myDownloadHook
    onLoadFinished      myLoadFinishedHook
    onNavigationRequest def
    onNewWebView        def
    onNewWindow         def
    onResourceOpened    def
    onTitleChanged      def

-- Web settings (cf Graphic.Gtk.WebKit.WebSettings)
    WS.modify webSettingsMonospaceFontFamily               $ const "consolas"
    WS.modify webSettingsEnableDeveloperExtras             $ const True
    WS.modify webSettingsEnablePlugins                     $ const False
    WS.modify webSettingsEnablePrivateBrowsing             $ const False
    WS.modify webSettingsEnableScripts                     $ const False
    WS.modify webSettingsEnableUniversalAccessFromFileUris $ const True
    WS.modify webSettingsEnableXssAuditor                  $ const True
    WS.modify webSettingsEnablePageCache                   $ const True
    WS.modify webSettingsJSCanOpenWindowAuto               $ const True
    WS.modify webSettingsSpellCheckingLang                 $ const $ Just "en_US"
    WS.modify webSettingsUserAgent                         $ const firefoxUserAgent

-- Scroll position in status bar
    setupScrollWidget =<< getObject castToLabel "scroll"

-- Zoom level in status bar
    setupZoomWidget =<< getObject castToLabel "zoom"

-- Load progress in status bar
    setupProgressWidget =<< getObject castToLabel "progress"

-- Current URI in status bar
    setupURIWidget defaultURIColors defaultSecureURIColors =<< getObject castToLabel "uri"

-- Session manager
    --setupSession browser

-- Favicon
    --_ <- on webView iconLoaded $ \uri -> do something

    return ()
