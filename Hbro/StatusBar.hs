{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Hbro.StatusBar where

-- {{{ Imports
import           Hbro
import           Hbro.Keys                                as Key
import           Hbro.Logger

import           Control.Lens.Getter
import           Control.Monad.Trans.Resource

import           Graphics.Rendering.Pango.Extended
import           Graphics.Rendering.Pango.Layout

import           Graphics.UI.Gtk.Display.Label
import           Graphics.UI.Gtk.General.General.Extended
import           Graphics.UI.Gtk.Misc.Adjustment
import           Graphics.UI.Gtk.WebKit.WebView

import           Network.URI                              as N

import           System.Glib.Attributes.Extended
-- }}}


-- | Write current scroll position in the given Label.
installScrollWidget :: (ControlIO m, MonadResource m, MonadReader r m, Has MainView r) => Label -> m ()
installScrollWidget widget = do
    gAsync $ labelSetAttributes widget [AttrForeground {paStart = 0, paEnd = -1, paColor = gray}]

    mainView <- ask
    addHandler (mainView^.scrolledHandlerL) . const $ do
        adjustment <- getAdjustment Vertical $ mainView^.scrollWindowL
        current    <- get adjustment adjustmentValue
        lower      <- get adjustment adjustmentLower
        upper      <- get adjustment adjustmentUpper
        page       <- get adjustment adjustmentPageSize

        case upper-lower-page of
            0 -> gAsync $ labelSetText widget (asText "ALL")
            x -> gAsync . labelSetText widget $ show ((round $ current/x*100) :: Int) ++ "%"

    gAsync $ labelSetText widget (asText "0%")

-- | /!\\ Doesn't work for now.
-- Write current zoom level in the given Label.
installZoomWidget :: (ControlIO m, MonadResource m, MonadReader r m, Has MainView r) => Label -> m ()
installZoomWidget widget = do
    gAsync $ labelSetAttributes widget [AttrForeground {paStart = 0, paEnd = -1, paColor = Color 65535 65535 65535}]
    mainView <- ask
    getWebView >>= \w -> get w webViewZoomLevel >>= updateZoomLabel
    void $ addHandler (mainView^.zoomLevelChangedHandlerL) updateZoomLabel
  where updateZoomLabel = gAsync . labelSetMarkup widget . escapeMarkup . show


-- | Write current keystrokes state in the given 'Label'
installKeyStrokesWidget :: (ControlIO m, MonadResource m, MonadReader r m, Has (Signal KeyMapPressed) r) => Label -> m ()
installKeyStrokesWidget widget = do
    io $ labelSetAttributes widget [AttrForeground {paStart = 0, paEnd = -1, paColor = yellow}]
    (keySignal :: Signal KeyMapPressed) <- ask
    void . addHandler keySignal $ \(strokes, isBound) -> gAsync $ do
      labelSetText widget . unwords $ map describe strokes
      labelSetAttributes widget [AttrForeground {paStart = 0, paEnd = -1, paColor = green <| isBound |> red}]


-- | Write current load progress in the given 'Label'.
installProgressWidget :: (ControlIO m, MonadLogger m, MonadResource m, MonadReader r m, Has MainView r) => Label -> m ()
installProgressWidget widget = do
    mainView <- ask
-- Load started
    addHandler (mainView^.loadStartedHandlerL) $ \_ -> gAsync $ do
        labelSetAttributes widget [AttrForeground {paStart = 0, paEnd = -1, paColor = red}]
        labelSetText widget (asText "0%")
-- Progress changed
    addHandler (mainView^.progressChangedHandlerL) $ \progress -> gAsync $ do
        labelSetAttributes widget [AttrForeground {paStart = 0, paEnd = -1, paColor = yellow}]
        labelSetText widget $ tshow progress ++ "%"
-- Load finished
    addHandler (mainView^.loadFinishedHandlerL) $ \_ -> gAsync $ do
        labelSetAttributes widget [AttrForeground {paStart = 0, paEnd = -1, paColor = green}]
        labelSetText widget (asText "100%")
-- Error
    addHandler (mainView^.loadFailedHandlerL) $ \(_uri, _e) -> gAsync $ do
        labelSetAttributes widget [AttrForeground {paStart = 0, paEnd = -1, paColor = red}]
        labelSetText widget $ asText "100%"

    return ()


-- | Write current URI, or the destination of a hovered link, in the given Label.
installURIWidget :: (ControlIO m, MonadResource m, MonadReader r m, Has MainView r, MonadLogger m, MonadCatch m)
                 => URIColors -> URIColors -> Label -> m ()
installURIWidget normalColors secureColors widget = do
    mainView <- ask
-- URI changed
    addHandler (mainView^.uriChangedHandlerL) $ labelSetURI normalColors secureColors widget
-- Link hovered
    addHandler (mainView^.linkHoveredHandlerL) $ \(uri, _title) ->
        labelSetURI normalColors secureColors widget uri
-- Link unhovered
    addHandler (mainView^.linkUnhoveredHandlerL) $ \_ -> void . logErrors $
        labelSetURI normalColors secureColors widget =<< getCurrentURI

    return ()


-- |
labelSetURI :: (MonadIO m) => URIColors -> URIColors -> Label -> URI -> m ()
labelSetURI normalColors secureColors widget uri = gAsync $ do
    let colors = case uriScheme uri of
          "https:" -> secureColors
          _        -> normalColors

    let i:j:k:l:_ = map length [
          uriScheme uri,
          maybe [] uriRegName (uriAuthority uri),
          uriPath uri,
          uriQuery uri]

    labelSetAttributes widget
        [ AttrWeight{     paStart = 0,         paEnd = -1,          paWeight = WeightBold }
        , AttrForeground{ paStart = 0,         paEnd = i+2,         paColor = mScheme colors }
        , AttrForeground{ paStart = i+2,       paEnd = i+2+j,       paColor = mHost colors }
        , AttrForeground{ paStart = i+2+j,     paEnd = i+2+j+k,     paColor = mPath colors }
        , AttrForeground{ paStart = i+2+j+k,   paEnd = i+2+j+k+l,   paColor = mQuery colors }
        , AttrForeground{ paStart = i+2+j+k+l, paEnd = -1,          paColor = mFragment colors }
        ]

    labelSetText widget (show uri)


data URIColors = URIColors
    { mScheme   :: Color
    , mHost     :: Color
    , mPort     :: Color
    , mUser     :: Color
    , mPath     :: Color
    , mQuery    :: Color
    , mFragment :: Color
    }

defaultURIColors :: URIColors
defaultURIColors = URIColors
    { mScheme   = Color 20000 20000 20000
    , mHost     = Color 50000 50000 50000
    , mPort     = Color 65535     0     0
    , mUser     = Color     0 65535     0
    , mPath     = Color 20000 20000 20000
    , mQuery    = Color 20000 20000 20000
    , mFragment = Color 10000 10000 65535
    }


defaultSecureURIColors :: URIColors
defaultSecureURIColors = defaultURIColors {
    mHost     = Color 50000 50000     0
}
