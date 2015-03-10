{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Data.Aeson.Extended (module X, module Data.Aeson.Extended) where

import           ClassyPrelude

import           Control.Monad.Except

import           Data.Aeson               as X
import           Data.Aeson.Encode.Pretty as X
import qualified Data.ByteString.Lazy     as Lazy


decodeM :: (FromJSON a, MonadError Text m) => Lazy.ByteString -> m a
decodeM = either (throwError . pack) return . eitherDecode
