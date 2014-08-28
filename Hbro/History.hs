-- | Designed to be imported as @qualified@.
module Hbro.History
    ( Entry(..)
    , log
    , log'
    , add
    , add'
    , entry
    , Hbro.History.select
    , Hbro.History.select'
    ) where

-- {{{ Imports
-- import Hbro
import           Hbro.Core
import           Hbro.Error
import           Hbro.Gui
import           Hbro.Logger
import           Hbro.Misc
import           Hbro.Prelude

import           Control.Monad.Reader

import           Data.Time

import           Filesystem           (IOMode (..), getAppDataDirectory,
                                       withTextFile)

import           Network.URI

import           Text.Parsec          hiding (many)
import           Text.Parsec.Text
-- }}}

-- {{{ Type definitions
data Entry = Entry
    { _time  :: LocalTime
    , _uri   :: URI
    , _title :: Text
    }

instance Describable Entry where
    describe (Entry time uri title) = unwords [pack (formatTime defaultTimeLocale dateFormat time), tshow uri, title]
-- }}}

dateFormat :: String
dateFormat = "%F %T"

-- Error message
-- invalidHistoryEntry :: Text -> Text
-- invalidHistoryEntry = ("Invalid history entry: " ++)

getHistoryFile :: (BaseIO m) => m FilePath
getHistoryFile = getAppDataDirectory "hbro" >/> "history"

-- | Log current visited page to history database
log :: (ControlIO m, MonadReader r m, HasGUI r, MonadError Text m) => m ()
log = log' =<< getHistoryFile

-- | Like 'log', but you can specify the history file path
log' :: (ControlIO m, MonadReader r m, HasGUI r, MonadError Text m) => FilePath -> m ()
log' file = do
    uri      <- getCurrentURI
    title    <- getPageTitle
    timeZone <- io $ utcToLocalTime <$> getCurrentTimeZone
    now      <- io $ timeZone <$> getCurrentTime

    add' file (Entry now uri title)

-- | Add a new entry to history database
add :: (ControlIO m, MonadError Text m) => Entry -> m ()
add newEntry = (`add'` newEntry) =<< getHistoryFile

-- | Like 'add', but you can specify the history file path
add' :: (ControlIO m, MonadError Text m) => FilePath -> Entry -> m ()
add' file newEntry = do
    debugM "hbro.history" $ "Adding new entry <" ++ tshow (_uri newEntry) ++ "> to history file <" ++ fpToText file ++ ">"
    handleIO (throwError . tshow) . io $ withTextFile file AppendMode (`hPutStrLn` describe newEntry)

-- | Try to parse a Text into a history Entry.
entry :: Parser Entry
entry = do
    spaces
    time1 <- some $ noneOf " "
    some space
    time2 <- some $ noneOf " "
    time  <- maybe mzero return $ parseTime defaultTimeLocale dateFormat (unwords [time1, time2])
    some space
    u <- some $ satisfy isAllowedInURI
    uri <- maybe mzero return $ parseURI u
    some space
    tags <- many (noneOf "\n")
    return $ Entry time uri (pack tags)

-- | Open a dmenu with all (sorted alphabetically) history entries, and return the user's selection, if any
select :: (ControlIO m, MonadError Text m)
       => [Text]            -- ^ dmenu's commandline options
       -> m Entry           -- ^ Selected history entry, if any
select dmenuOptions = (`select'` dmenuOptions) =<< getHistoryFile

-- | Like 'select', but you can specify the history file path
select' :: (ControlIO m, MonadError Text m) => FilePath -> [Text] -> m Entry
select' file dmenuOptions = do
    either (throwError . tshow) return . runParser entry () "(unknown)" =<< dmenu dmenuOptions . unlines . reverse . sort . ordNub . lines =<< readFile file
