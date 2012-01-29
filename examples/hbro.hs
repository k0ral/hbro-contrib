{-# LANGUAGE DoRec #-}
module Main where

-- {{{ Imports
import qualified Hbro.Bookmarks as Bookmarks
import qualified Hbro.BookmarksQueue as Queue
import Hbro.Clipboard
import Hbro.Config
import Hbro.Core
import qualified Hbro.Download as Download
import Hbro.Gui
import Hbro.Hbro
import qualified Hbro.History as History
import Hbro.Keys
import Hbro.Misc
import qualified Hbro.Prompt as Prompt
import Hbro.Session
import Hbro.Socket
import Hbro.StatusBar
import Hbro.Types
import Hbro.Util
import Hbro.WebSettings

import Control.Monad hiding(forM_, mapM_)

import Data.Foldable
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

import Network.URI

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

-- {{{ Configuration structure
-- Main function, expected to call launchHbro.
-- You can add custom tasks before & after calling it.
main :: IO ()
main = launchHbro myConfig

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override will     
-- use the defaults defined in Hbro.Types.Config.
myConfig :: Config
myConfig = defaultConfig {
    mSocketDir        = mySocketDirectory,
    mUIFile           = myUIFile,
    mHomePage         = myHomePage,
    mWebSettings      = myWebSettings,
    mHooks            = myHooks
}

-- Commented fields are using default values
myHooks = defaultHooks {
--  mBackForward     = myBackForward,
    mDownload        = myDownloadHook,
--  mFormResubmitted = myFormResubmitted,
--  mFormSubmitted   = myFormSubmitted,
    mKeyPressed      = manageSequentialKeys (defaultKeyHandler myKeys) >=> void . (printInLabel "keys"),
--  mLinkClicked     = myLinkClicked,
    mLoadFinished    = myLoadFinished,
--  mMIMEDisposition = myMIMEDisposition,
--  mNewWindow       = myNewWindowHook,
--  mOtherNavigation = myOtherNavigation,
--  mReload          = myReload,
    mStartUp         = myStartUp
--  mTitleChanged    = myTitleChanged
}
-- }}}

-- {{{ Constant parameters
myHomePage = "https://duckduckgo.com"

mySocketDirectory, myUIFile, myHistoryFile, myBookmarksFile :: RefDirs -> FilePath
mySocketDirectory             = mTemporary
myUIFile          directories = (mConfiguration directories) </> "ui.xml"
myHistoryFile     directories = (mData directories) </> "history"
myBookmarksFile   directories = (mData directories) </> "bookmarks"
-- }}}

-- {{{ Hooks
myDownloadHook :: URI -> String -> Int -> K ()
myDownloadHook uri filename _size = io $ do
    home <- getHomeDirectory 
    Download.aria uri home filename

myLoadFinished :: K ()
myLoadFinished = 
    withURI $ \uri -> do
      withTitle $ \title -> io $ do
        timeZone <- utcToLocalTime `fmap` getCurrentTimeZone
        now      <- timeZone `fmap` getCurrentTime
   
        History.add myHistoryFile (History.Entry now uri title) >> return ()
        
-- {{{ Keys
-- Note that this example is suited for an azerty keyboard.
myKeys :: KeysList
myKeys = defaultKeyBindings ++ [
-- Browse
    ("C-<Left>",      goBackList    ["-l", "10"] >>= mapM_ loadURI),
    ("C-<Right>",     goForwardList ["-l", "10"] >>= mapM_ loadURI),
    ("C-g",           Prompt.read "Google search" [] ((mapM_ loadURI . parseURI . ("https://www.google.com/search?q=" ++)))),
-- Copy/paste
    ("C-y",           withURI $ io . toClipboard . show),
    ("M-y",           withTitle $ io . toClipboard),
    ("C-p",           withClipboard $ mapM_ loadURI . parseURIReference),
    ("M-p",           withClipboard $ \uri -> io $ spawn "hbro" ["-u", uri]),
-- Bookmarks
    ("C-d",           Prompt.read "Bookmark with tags:" [] $ \tags -> do 
        withURI $ (\uri -> (io . void . Bookmarks.add myBookmarksFile . Bookmarks.Entry uri . words) tags)
    ),
    ("C-D",           Prompt.read "Bookmark all instances with tag:" [] $ \tags -> 
        (map parseURI `fmap` sendCommandToAll "GET_URI")
        >>= mapM (mapM_ $ \uri -> (io . Bookmarks.add myBookmarksFile) $ Bookmarks.Entry uri (words tags)) 
        >> (withURI $ \uri -> (io . void . Bookmarks.add myBookmarksFile) $ Bookmarks.Entry uri (words tags)) 
    ),
    ("M-d",           io $ Bookmarks.deleteWithTag myBookmarksFile ["-l", "10"]),
    ("C-l",           io (Bookmarks.select        myBookmarksFile ["-l", "10"]) >>= mapM_ (mapM_ loadURI . parseURIReference)),
    ("C-L",           io (Bookmarks.selectTag     myBookmarksFile ["-l", "10"]) >>= mapM_ (\uris -> mapM (\uri -> io $ spawn "hbro" ["-u", (show uri)]) uris >> return ())),
--    ("C-q"),           webViewGetUri webView >>= maybe (return ()) (Queue.append),
--    ("M-q"),           \b -> do
--        uri <- Queue.popFront
--        loadURI uri b),

-- History
    ("C-h",           io (History.select myHistoryFile ["-l", "10"]) >>= mapM_ loadURI . (return . (History.mURI) =<<))
    
-- Session
    --("M-l"),           loadFromSession ["-l", "10"])
    ]
-- }}}

-- {{{ Web settings
-- Commented out lines correspond to default values.
myWebSettings :: [AttrOp WebSettings]
myWebSettings = [
--  SETTING                                        VALUE 
    --webSettingsCursiveFontFamily              := "serif",
    --webSettingsDefaultFontFamily              := "sans-serif",
    --webSettingsFantasyFontFamily              := ,
    --webSettingsMonospaceFontFamily            := "monospace",
    --webSettingsSansFontFamily                 := "sans-serif",
    --webSettingsSerifFontFamily                := "serif",
    --webSettingsDefaultFontSize                := ,
    --webSettingsDefaultMonospaceFontSize       := 10,
    --webSettingsMinimumFontSize                := 5,
    --webSettingsMinimumLogicalFontSize         := 5,
    --webSettingsAutoLoadImages                 := True,
    --webSettingsAutoShrinkImages               := True,
    --webSettingsDefaultEncoding                := "iso-8859-1",
    --webSettingsEditingBehavior                := EditingBehaviorWindows,
    --webSettingsEnableCaretBrowsing              := False,
    webSettingsEnableDeveloperExtras            := True,
    --webSettingsEnableHtml5Database              := True,
    --webSettingsEnableHtml5LocalStorage          := True,
    --webSettingsEnableOfflineWebApplicationCache := True,
    webSettingsEnablePlugins                    := True,
    webSettingsEnablePrivateBrowsing            := False, -- Experimental
    webSettingsEnableScripts                    := False,
    --webSettingsEnableSpellChecking              := False,
    webSettingsEnableUniversalAccessFromFileUris := True,
    webSettingsEnableXssAuditor                 := True,
    --webSettingsEnableSiteSpecificQuirks       := False,
    --webSettingsEnableDomPaste                 := False,
    --webSettingsEnableDefaultContextMenu       := True,
    webSettingsEnablePageCache                  := True,
    --webSettingsEnableSpatialNavigation        := False,
    --webSettingsEnforce96Dpi                   := ,
    webSettingsJSCanOpenWindowAuto              := True,
    --webSettingsPrintBackgrounds               := True,
    --webSettingsResizableTextAreas             := True,
    webSettingsSpellCheckingLang                := Just "en_US",
    --webSettingsTabKeyCyclesThroughElements    := True,
    webSettingsUserAgent                        := firefoxUserAgent
    --webSettingsUserStylesheetUri              := Nothing,
    --webSettingsZoomStep                       := 0.1
    ]
-- }}}

-- {{{ Setup
myStartUp :: K ()
myStartUp = do
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
-- }}}
