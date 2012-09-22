module Hbro.Download where

-- {{{ Imports
import Hbro.Util

import Control.Monad.IO.Class

import Network.URI

import System.FilePath
-- }}}


aria, wget, axel :: (MonadIO m) => IO FilePath -> URI -> String -> m ()
aria path' uri filename = io $ path' >>= \destination -> spawn "aria2c" [show uri, "-d", destination, "-o", filename]
wget path' uri filename = io $ path' >>= \destination -> spawn "wget"   [show uri, "-O", destination </> filename]
axel path' uri filename = io $ path' >>= \destination -> spawn "axel"   [show uri, "-o", destination </> filename]
