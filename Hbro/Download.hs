{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Hbro.Download where

-- {{{ Imports
import           Hbro.Error
import           Hbro.Prelude

import           Network.URI

import           System.FilePath
import           System.Process
-- }}}


aria, wget, axel :: (ControlIO m, MonadCatch m)
                 => FilePath -- ^ Destination directory
                 -> URI      -- ^ URI to download
                 -> Text     -- ^ Destination file name
                 -> m ()
aria (pack -> destination) (show -> uri) outputFile
  = downloadWith "aria2c" [uri, "-d", destination, "-o", outputFile, "-q"] outputFile

wget destination (show -> uri) outputFile
  = downloadWith "wget" [uri, "-O", dest] outputFile
    where dest = pack (destination </> unpack outputFile)

axel destination (show -> uri) outputFile
  = downloadWith "axel" [uri, "-o", dest] outputFile
    where dest = pack (destination </> unpack outputFile)

downloadWith :: (ControlIO m, MonadCatch m) => Text -> [Text] -> Text -> m ()
downloadWith (unpack -> program) (map unpack -> args) (unpack -> outputFile) = handleIO (io . print) . io $ do
    callProcess "notify-send" ["Download started", outputFile]
    callProcess program args
    callProcess "notify-send" ["Download finished", outputFile]
