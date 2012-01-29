module Hbro.Clipboard where

-- {{{ Imports
import Hbro.Core
import Hbro.Types
import Hbro.Util

import Graphics.UI.Gtk.General.Clipboard
-- }}}


-- | Write given String to primary clipboard.
toClipboard :: String -> IO ()
toClipboard text = clipboardGet selectionPrimary >>= (`clipboardSetText` text)
    
withClipboard :: (String -> K ()) -> K ()
withClipboard callback = do
    clip <- io $ clipboardGet selectionPrimary 
    mapK2 (clipboardRequestText clip) (maybe (return ()) callback)
