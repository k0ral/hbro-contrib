{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Data.Aeson.Extended (module X, module Data.Aeson.Extended) where

import           ClassyPrelude

import           Data.Aeson               as X
import           Data.Aeson.Encode.Pretty as X
import qualified Data.ByteString.Lazy     as Lazy


data JsonException = UnableDecode String deriving(Eq)

instance Exception JsonException
instance Show JsonException where
  show (UnableDecode s) = s

decodeM :: (FromJSON a, MonadThrow m) => Lazy.ByteString -> m a
decodeM = either (throwM . UnableDecode) return . eitherDecode
