module Hbro.Misc where

-- {{{ Imports
import           Hbro
import           Hbro.Gui                                  as Gui

import           Graphics.UI.Gtk.WebKit.WebBackForwardList
import           Graphics.UI.Gtk.WebKit.WebHistoryItem
import           Graphics.UI.Gtk.WebKit.WebView

import           Network.URI.Monadic

import           Safe

import           System.Process
-- }}}


-- | Open dmenu with given input and return selected entry.
-- This will block effectively the current thread.
dmenu :: (ControlIO m, MonadError Text m)
      => [Text]    -- ^ dmenu's commandline options
      -> Text      -- ^ dmenu's input
      -> m Text    -- ^ Selected entry
dmenu options input = handleIO (\_ -> throwError "Dmenu canceled.") $ do
    (in_, out, err, pid) <- io $ runInteractiveProcess "dmenu" (map unpack options) Nothing Nothing
    hPut in_ input
    io $ hClose in_

    output <- hGetLine out

    io $ hClose out >> hClose err >> (void $ waitForProcess pid)
    return output


-- | List preceding URIs in dmenu and let the user select which one to load.
goBackList :: (ControlIO m, MonadReader r m, HasGUI r, MonadError Text m) => [Text] -> m URI
goBackList dmenuOptions = do
    list           <- io . webViewGetBackForwardList =<< Gui.get webViewL
    n              <- io $ webBackForwardListGetBackLength list
    backList       <- io $ webBackForwardListGetBackListWithLimit list n
    dmenuList      <- io $ mapM itemToEntry backList

    parseURIReference . headDef "" . words =<< (dmenu dmenuOptions . unlines . catMaybes) dmenuList


-- | List succeeding URIs in dmenu and let the user select which one to load.
goForwardList :: (ControlIO m, MonadReader r m, HasGUI r, MonadError Text m) => [Text] -> m URI
goForwardList dmenuOptions = do
    list        <- io . webViewGetBackForwardList =<< Gui.get webViewL
    n           <- io $ webBackForwardListGetForwardLength list
    forwardList <- io $ webBackForwardListGetForwardListWithLimit list n
    dmenuList   <- io $ mapM itemToEntry forwardList

    parseURIReference . headDef "" . words =<< (dmenu dmenuOptions . unlines . catMaybes) dmenuList


itemToEntry :: WebHistoryItem -> IO (Maybe Text)
itemToEntry item = do
    title <- webHistoryItemGetTitle item
    uri   <- webHistoryItemGetUri   item
    case uri of
        Just u -> return $ Just (u ++ " | " ++ (maybe "Untitled" id title))
        _      -> return Nothing
