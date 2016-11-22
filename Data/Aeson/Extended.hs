{-# LANGUAGE FlexibleContexts  #-}
module Data.Aeson.Extended (module X, module Data.Aeson.Extended) where

-- {{{ Imports
import           Control.Exception.Safe
import           Control.Monad (MonadPlus(..))

import           Data.Aeson               as X
import           Data.Aeson.Encode.Pretty as X
import qualified Data.ByteString.Lazy     as Lazy
import           Data.Text                as T

import           Network.URI
-- }}}

data JsonException = UnableDecode String deriving(Eq, Show)

instance Exception JsonException where
  displayException (UnableDecode s) = s

-- | Trivial wrapper around 'URI', used to avoid orphan instances.
newtype WrappedURI = WrappedURI { unwrapURI :: URI }

instance FromJSON WrappedURI where
  parseJSON (String s) = maybe mzero (return . WrappedURI) . parseURIReference $ T.unpack s
  parseJSON _          = mzero

instance ToJSON WrappedURI where
  toJSON (WrappedURI uri) = toJSON $ show uri

decodeM :: (FromJSON a, MonadThrow m) => Lazy.ByteString -> m a
decodeM = either (throwM . UnableDecode) return . eitherDecode
