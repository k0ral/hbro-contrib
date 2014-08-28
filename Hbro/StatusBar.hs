{-# LANGUAGE ViewPatterns #-}
module Hbro.StatusBar where

-- {{{ Imports
import Hbro.Keys as Key
import Hbro.Gui as Gui
import Hbro.Prelude hiding(on)

import Control.Monad.Reader hiding(forM_, mapM_)

import Graphics.Rendering.Pango.Enums
import Graphics.Rendering.Pango.Layout

import Graphics.UI.Gtk.Display.Label
--import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Misc.Adjustment
import Graphics.UI.Gtk.Scrolling.ScrolledWindow
import Graphics.UI.Gtk.WebKit.WebView

import Network.URI as N

import System.Glib.Signals
-- }}}


-- | Write current scroll position in the given Label.
installScrollWidget :: (BaseIO m, MonadReader r m, HasGUI r) => Label -> m ()
installScrollWidget widget = do
    adjustment <- io . scrolledWindowGetVAdjustment =<< Gui.get scrollWindowL
    io $ labelSetAttributes widget [AttrForeground {paStart = 0, paEnd = -1, paColor = gray}]

    io . void . onValueChanged adjustment $ do
        current <- adjustmentGetValue    adjustment
        lower   <- adjustmentGetLower    adjustment
        upper   <- adjustmentGetUpper    adjustment
        page    <- adjustmentGetPageSize adjustment

        case upper-lower-page of
            0 -> labelSetText widget (asText "ALL")
            x -> labelSetText widget $ show (round $ current/x*100) ++ "%"

    io $ labelSetText widget (asText "0%")

-- | /!\\ Doesn't work for now.
-- Write current zoom level in the given Label.
installZoomWidget :: (BaseIO m, MonadReader r m, HasGUI r) => Label -> m ()
installZoomWidget widget = do
    io $ labelSetAttributes widget [AttrForeground {paStart = 0, paEnd = -1, paColor = Color 65535 65535 65535}]
    Gui.get webViewL >>= io . webViewGetZoomLevel >>= io . labelSetMarkup widget . escapeMarkup . show

-- | Write current keystrokes state in the given 'Label'
installKeyStrokesWidget :: (BaseIO m, MonadReader r m, HasGUI r, HasHooks n r, BaseIO n) => Label -> m ()
installKeyStrokesWidget widget = do
    io $ labelSetAttributes widget [AttrForeground {paStart = 0, paEnd = -1, paColor = yellow}]

    Key.set onKeyPressedL $ io . labelSetText widget . unwords . map describe

-- | Write current load progress in the given 'Label'.
installProgressWidget :: (BaseIO m, MonadReader r m, HasGUI r) => Label -> m ()
installProgressWidget widget = do
    wv <- Gui.get webViewL
-- Load started
    io . void . on wv loadStarted $ \_ -> do
        labelSetAttributes widget [AttrForeground {paStart = 0, paEnd = -1, paColor = red}]
        labelSetText widget (asText "0%")
-- Progress changed
    io . void . on wv progressChanged $ \progress -> do
        labelSetAttributes widget [AttrForeground {paStart = 0, paEnd = -1, paColor = yellow}]
        labelSetText widget $ tshow progress ++ "%"
-- Load finished
    io . void . on wv loadFinished $ \_ -> do
        labelSetAttributes widget [AttrForeground {paStart = 0, paEnd = -1, paColor = green}]
        labelSetText widget (asText "100%")
-- Error
    io . void . on wv loadError $ \_ (asText -> _a) _ -> do
        labelSetAttributes widget [AttrForeground {paStart = 0, paEnd = -1, paColor = red}]
        labelSetText widget (asText "ERROR")
        return False

    return ()


-- | Write current URI, or the destination of a hovered link, in the given Label.
installURIWidget :: (BaseIO m, MonadReader r m, HasGUI r) => URIColors -> URIColors -> Label -> m ()
installURIWidget normalColors secureColors widget = do
    wv <- Gui.get webViewL
-- URI changed
    _ <- io $ on wv loadCommitted $ \_ ->
        (mapM_ (labelSetURI normalColors secureColors widget)) =<< ((>>= N.parseURIReference) `fmap` (webViewGetUri wv))
-- Link (un)hovered
    _ <- io $ on wv hoveringOverLink $ \_title hoveredURI -> do
        uri <- webViewGetUri wv

        forM_ (hoveredURI >>= N.parseURIReference) $ labelSetURI normalColors secureColors widget
        unless (isJust hoveredURI) $ forM_ (uri >>= N.parseURIReference) (labelSetURI normalColors secureColors widget)

    return ()


-- |
labelSetURI :: URIColors -> URIColors -> Label -> URI -> IO ()
labelSetURI normalColors secureColors widget uri = do
    let colors = case uriScheme uri of
          "https:" -> secureColors
          _        -> normalColors

    let i:j:k:l:_ = map length [
          uriScheme uri,
          maybe [] uriRegName (uriAuthority uri),
          uriPath uri,
          uriQuery uri]

    labelSetAttributes widget $ [
        AttrWeight{     paStart = 0,         paEnd = -1,          paWeight = WeightBold },
        AttrForeground{ paStart = 0,         paEnd = i+2,         paColor = mScheme colors },
        AttrForeground{ paStart = i+2,       paEnd = i+2+j,       paColor = mHost colors },
        AttrForeground{ paStart = i+2+j,     paEnd = i+2+j+k,     paColor = mPath colors },
        AttrForeground{ paStart = i+2+j+k,   paEnd = i+2+j+k+l,   paColor = mQuery colors },
        AttrForeground{ paStart = i+2+j+k+l, paEnd = -1,          paColor = mFragment colors }]

    labelSetText widget (show uri)


data URIColors = URIColors {
    mScheme     :: Color,
    mHost       :: Color,
    mPort       :: Color,
    mUser       :: Color,
    mPath       :: Color,
    mQuery      :: Color,
    mFragment   :: Color
}

defaultURIColors :: URIColors
defaultURIColors = URIColors {
    mScheme   = Color 20000 20000 20000,
    mHost     = Color 50000 50000 50000,
    mPort     = Color 65535     0     0,
    mUser     = Color     0 65535     0,
    mPath     = Color 20000 20000 20000,
    mQuery    = Color 20000 20000 20000,
    mFragment = Color 10000 10000 65535
}


defaultSecureURIColors :: URIColors
defaultSecureURIColors = defaultURIColors {
    mHost     = Color 50000 50000     0
}
