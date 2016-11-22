{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
-- | Designed to be imported as @qualified@.
module Hbro.History
    ( Entry(..)
    , HistoryException(..)
    , log
    , log'
    , add
    , add'
    , Hbro.History.select
    , Hbro.History.select'
    ) where

-- {{{ Imports
import           Hbro hiding(log)
import           Hbro.Logger
import           Hbro.Misc

import           Data.Aeson.Extended
import           Data.IOData
import qualified Data.Set            as Set
import           Data.Time

import           Network.URI

import           System.Directory
import           System.FilePath

import           Text.Read
-- }}}

-- {{{ Type definitions
data Entry = Entry
    { _time  :: UTCTime
    , _uri   :: URI
    , _title :: Text
    }

deriving instance Eq Entry
instance Ord Entry where
  compare (Entry t u _) (Entry t' u' _)
    | u == u' = EQ
    | otherwise = compare t t'

instance Describable Entry where
    describe (Entry time uri title) = unwords [pack (formatTime defaultTimeLocale dateFormat time), show uri, title]

instance FromJSON Entry where
    parseJSON (Object v) = Entry <$> v .: "time" <*> (unwrapURI <$> v .: "uri") <*> v .: "title"
    parseJSON _          = mzero

instance ToJSON Entry where
    toJSON (Entry time uri title) = object ["time" .= time, "uri" .= WrappedURI uri, "title" .= title]

data HistoryException = InvalidSelection deriving(Eq, Show)

instance Exception HistoryException where
  displayException InvalidSelection = "Invalid history item selected."
-- }}}

dateFormat :: String
dateFormat = "%F %T"

getHistoryFile :: (BaseIO m) => m FilePath
getHistoryFile = getAppUserDataDirectory "hbro" >/> "history"

-- | Log current visited page to history database
log :: (ControlIO m, MonadLogger m, MonadReader r m, Has MainView r, MonadCatch m, Alternative m) => m ()
log = log' =<< getHistoryFile

-- | Like 'log', but you can specify the history file path
log' :: (ControlIO m, MonadLogger m, MonadReader r m, Has MainView r, MonadCatch m, Alternative m) => FilePath -> m ()
log' file = do
    uri      <- getCurrentURI
    title    <- getPageTitle
    now      <- io $ zonedTimeToUTC <$> getZonedTime

    add' file (Entry now uri title)

-- | Add a new entry to history database
add :: (ControlIO m, MonadLogger m, MonadCatch m, Alternative m) => Entry -> m ()
add newEntry = (`add'` newEntry) =<< getHistoryFile

-- | Like 'add', but you can specify the history file path
add' :: (ControlIO m, MonadLogger m, MonadCatch m, Alternative m) => FilePath -> Entry -> m ()
add' file newEntry = do
    debug $ "Adding new entry <" <> show (_uri newEntry) <> "> to history file <" <> pack file <> ">"
    tryIO . io . copyFile file $ file <.> "bak"
    entries <- (decodeM =<< readFile file) <|> return Set.empty

    writeFile file . encodePretty $ Set.insert newEntry entries

-- | Open a dmenu with all (sorted alphabetically) history entries, and return the user's selection, if any
select :: (ControlIO m, MonadThrow m) => m Entry
select = (`select'` defaultDmenuOptions) =<< getHistoryFile

-- | Like 'select', but you can specify the history file path
select' :: (ControlIO m, MonadThrow m) => FilePath -> [Text] -> m Entry
select' file dmenuOptions = do
  entries <- zip [1..] . sortBy (flip (comparing _time)) <$> (decodeM =<< readFile file)
  result  <- dmenu dmenuOptions . unlines $ map (\(i :: Int, e :: Entry) -> show i <> " " <> describe e) entries
  maybe (throwM InvalidSelection) return ((`lookup` entries) =<< readMaybe . unpack =<< headMay (words result))
