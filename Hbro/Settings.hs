{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Hbro.Settings where

-- {{{ Import
import           Hbro.Gui.MainView
import           Hbro.Logger
import           Hbro.Prelude

import           Graphics.UI.Gtk.General.General.Extended
import           Graphics.UI.Gtk.WebKit.WebSettings
import           Graphics.UI.Gtk.WebKit.WebView

import           System.Glib.Attributes.Extended
-- }}}

-- | Disable HTML5 database & local storage, plugins and scripts.
setParanoidWebSettings :: (BaseIO m, MonadLogger m, MonadReader r m, Has MainView r) => m ()
setParanoidWebSettings = do
    s <- gSync . webViewGetWebSettings =<< getWebView
    --set s webSettingsEnablePrivateBrowsing		False, --  Experimental
-- Privacy
    set s webSettingsEnableHtml5Database              False
    set s webSettingsEnableHtml5LocalStorage          False
    set s webSettingsEnableOfflineWebApplicationCache False
    set s webSettingsEnableSiteSpecificQuirks         False
    set s webSettingsUserAgent                        firefoxUserAgent
-- Security
    set s webSettingsEnablePlugins                    False
    set s webSettingsEnableScripts                    False
    set s webSettingsJSCanOpenWindowAuto              False
    return ()

-- {{{ User agents
chromeUserAgent, epiphanyUserAgent, firefoxUserAgent, internetExplorerUserAgent, operaUserAgent, safariUserAgent :: String
chromeUserAgent           = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/535.11 (KHTML, like Gecko) Chrome/17.0.963.12 Safari/535.11"
epiphanyUserAgent         = "Mozilla/5.0 (X11; U; Linux x86_64; en-US) AppleWebKit/534.7 (KHTML, like Gecko) Epiphany/2.30.6 Safari/534.7"
firefoxUserAgent          = "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:26.0) Gecko/20100101 Firefox/26.0"
internetExplorerUserAgent = "Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.1; Trident/6.0)"
operaUserAgent            = "Opera/9.80 (X11; Linux x86_64; U; en) Presto/2.9.168 Version/11.50"
safariUserAgent           = "Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) AppleWebKit/533.20.25 (KHTML, like Gecko) Version/5.0.4 Safari/533.20.27"
-- }}}
