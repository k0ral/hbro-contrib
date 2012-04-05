module Hbro.Misc where

-- {{{ Imports
import Hbro.Core
import Hbro.Types
import Hbro.Util

import Data.Functor
import Data.Maybe

-- import Graphics.UI.Gtk.Display.Label
import Graphics.UI.Gtk.WebKit.WebBackForwardList
import Graphics.UI.Gtk.WebKit.WebHistoryItem
import Graphics.UI.Gtk.WebKit.WebView

import Network.URI

-- import System.IO
-- }}}


-- | List preceding URIs in dmenu and let the user select which one to load.
goBackList :: [String] -> K (Maybe URI)
goBackList dmenuOptions = do
    list           <- with (mWebView . mGUI) webViewGetBackForwardList
    n              <- io $ webBackForwardListGetBackLength list
    backList       <- io $ webBackForwardListGetBackListWithLimit list n
    dmenuList      <- io $ mapM itemToEntry backList
    
    (>>= (parseURIReference . head . words)) <$> (io . dmenu dmenuOptions . unlines . catMaybes) dmenuList
    

-- | List succeeding URIs in dmenu and let the user select which one to load.
goForwardList :: [String] -> K (Maybe URI)
goForwardList dmenuOptions = do
    list        <- with (mWebView . mGUI) webViewGetBackForwardList
    n           <- io $ webBackForwardListGetForwardLength list
    forwardList <- io $ webBackForwardListGetForwardListWithLimit list n
    dmenuList   <- io $ mapM itemToEntry forwardList
    
    (>>= (parseURIReference . head . words)) `fmap` (io . dmenu dmenuOptions . unlines . catMaybes) dmenuList


itemToEntry :: WebHistoryItem -> IO (Maybe String)
itemToEntry item = do
    title <- webHistoryItemGetTitle item
    uri   <- webHistoryItemGetUri   item
    case uri of
        Just u -> return $ Just (u ++ " | " ++ (maybe "Untitled" id title))
        _      -> return Nothing
