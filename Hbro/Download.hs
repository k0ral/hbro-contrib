module Hbro.Download where

-- {{{ Imports
import           Hbro.Prelude

import           Network.URI
-- }}}


aria, wget, axel :: (BaseIO m)
                 => FilePath -- ^ Destination directory
                 -> URI      -- ^ URI to download
                 -> Text     -- ^ Destination file name
                 -> m ()
aria destination uri filename = spawn "aria2c" [show uri, "-d", unpack $ fpToText destination, "-o", unpack filename]
wget destination uri filename = spawn "wget"   [show uri, "-O", unpack $ fpToText (destination </> fpFromText filename)]
axel destination uri filename = spawn "axel"   [show uri, "-o", unpack $ fpToText (destination </> fpFromText filename)]
