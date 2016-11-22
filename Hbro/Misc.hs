{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Hbro.Misc where

-- {{{ Imports
import           Hbro

import           Data.IOData

import           Graphics.UI.Gtk.WebKit.WebBackForwardList
import           Graphics.UI.Gtk.WebKit.WebHistoryItem
import           Graphics.UI.Gtk.WebKit.WebView

import           Network.URI.Extended

import           Safe

import           System.IO (hClose)
import           System.Process
-- }}}


-- | Open dmenu with given input and return selected entry.
-- This will block effectively the current thread.
dmenu :: (ControlIO m)
      => [Text]    -- ^ dmenu's commandline options
      -> Text      -- ^ dmenu's input
      -> m Text    -- ^ Selected entry
dmenu options input = do
    (in_, out, err, pid) <- io $ runInteractiveProcess "dmenu" (map unpack options) Nothing Nothing
    hPut in_ input
    io $ hClose in_

    output <- hGetLine out

    io $ hClose out >> hClose err >> void (waitForProcess pid)
    return output

defaultDmenuOptions :: [Text]
defaultDmenuOptions = ["-l", "10"]


-- | List preceding URIs in dmenu and let the user select which one to load.
goBackList :: (ControlIO m, MonadReader r m, Has MainView r, MonadThrow m) => m URI
goBackList = do
    list           <- io . webViewGetBackForwardList =<< getWebView
    n              <- io $ webBackForwardListGetBackLength list
    backList       <- io $ webBackForwardListGetBackListWithLimit list n
    dmenuList      <- io $ mapM itemToEntry backList

    parseURIReference . headDef "" . words =<< (dmenu defaultDmenuOptions . unlines . catMaybes) dmenuList


-- | List succeeding URIs in dmenu and let the user select which one to load.
goForwardList :: (ControlIO m, MonadReader r m, Has MainView r, MonadThrow m) => m URI
goForwardList = do
    list        <- io . webViewGetBackForwardList =<< getWebView
    n           <- io $ webBackForwardListGetForwardLength list
    forwardList <- io $ webBackForwardListGetForwardListWithLimit list n
    dmenuList   <- io $ mapM itemToEntry forwardList

    parseURIReference . headDef "" . words =<< (dmenu defaultDmenuOptions . unlines . catMaybes) dmenuList


itemToEntry :: WebHistoryItem -> IO (Maybe Text)
itemToEntry item = do
    title <- webHistoryItemGetTitle item
    uri   <- webHistoryItemGetUri   item
    case uri of
        Just u -> return $ Just (u <> " | " <> fromMaybe "Untitled" title)
        _      -> return Nothing
