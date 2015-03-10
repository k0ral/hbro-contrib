{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
-- | Designed to be imported as @qualified@.
module Hbro.History
    ( Entry(..)
    , log
    , log'
    , add
    , add'
    , Hbro.History.select
    , Hbro.History.select'
    ) where

-- {{{ Imports
import           Hbro
import           Hbro.Logger
import           Hbro.Misc

import           Data.Aeson.Extended
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Set             as Set
import           Data.Time

import           Filesystem           (copyFile, getAppDataDirectory)

import           Network.URI
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
    describe (Entry time uri title) = unwords [pack (formatTime defaultTimeLocale dateFormat time), tshow uri, title]

instance FromJSON Entry where
    parseJSON (Object v) = Entry <$> v .: "time" <*> v .: "uri" <*> v .: "title"
    parseJSON _          = mzero

instance ToJSON Entry where
    toJSON (Entry time uri title) = object ["time" .= time, "uri" .= uri, "title" .= title]

-- }}}

-- {{{ Util
readFileE :: (ControlIO m, MonadError Text m) => FilePath -> m Lazy.ByteString
readFileE = handleIO (throwError . tshow) . readFile

writeFileE :: (ControlIO m, MonadError Text m) => FilePath -> Lazy.ByteString -> m ()
writeFileE f x = handleIO (throwError . tshow) $ writeFile f x
-- }}}

dateFormat :: String
dateFormat = "%F %T"

getHistoryFile :: (BaseIO m) => m FilePath
getHistoryFile = getAppDataDirectory "hbro" >/> "history"

-- | Log current visited page to history database
log :: (ControlIO m, MonadLogger m, MonadReader r m, Has MainView r, MonadError Text m, Alternative m) => m ()
log = log' =<< getHistoryFile

-- | Like 'log', but you can specify the history file path
log' :: (ControlIO m, MonadLogger m, MonadReader r m, Has MainView r, MonadError Text m, Alternative m) => FilePath -> m ()
log' file = do
    uri      <- getCurrentURI
    title    <- getPageTitle
    now      <- io $ zonedTimeToUTC <$> getZonedTime

    add' file (Entry now uri title)

-- | Add a new entry to history database
add :: (ControlIO m, MonadLogger m, MonadError Text m, Alternative m) => Entry -> m ()
add newEntry = (`add'` newEntry) =<< getHistoryFile

-- | Like 'add', but you can specify the history file path
add' :: (ControlIO m, MonadLogger m, MonadError Text m, Alternative m) => FilePath -> Entry -> m ()
add' file newEntry = do
    debug $ "Adding new entry <" ++ tshow (_uri newEntry) ++ "> to history file <" ++ fpToText file ++ ">"
    tryIO . io . copyFile file $ file <.> "bak"
    entries <- (decodeM =<< readFileE file) <|> return Set.empty

    writeFileE file . encodePretty $ Set.insert newEntry entries

-- | Open a dmenu with all (sorted alphabetically) history entries, and return the user's selection, if any
select :: (ControlIO m, MonadError Text m) => m Entry
select = (`select'` defaultDmenuOptions) =<< getHistoryFile

-- | Like 'select', but you can specify the history file path
select' :: (ControlIO m, MonadError Text m) => FilePath -> [Text] -> m Entry
select' file dmenuOptions = do
  entries <- zip [1..] . sortBy (flip (comparing _time)) <$> (decodeM =<< readFileE file)
  result  <- dmenu dmenuOptions . unlines $ map (\(i :: Int, e :: Entry) -> tshow i ++ " " ++ describe e) entries
  maybe (throwError "Invalid history item selected") return ((`lookup` entries) =<< readMay =<< headMay (words result))
