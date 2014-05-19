module Hbro.Download where

-- {{{ Imports
import Hbro.Util

import Network.URI

import System.FilePath
-- }}}


aria, wget, axel :: (MonadBase IO m)
                 => FilePath -- ^ Destination directory
                 -> URI      -- ^ URI to download
                 -> String   -- ^ Destination file name
                 -> m ()
aria destination uri filename = spawn "aria2c" [show uri, "-d", destination, "-o", filename]
wget destination uri filename = spawn "wget"   [show uri, "-O", destination </> filename]
axel destination uri filename = spawn "axel"   [show uri, "-o", destination </> filename]
