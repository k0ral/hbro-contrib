-- | Designed to be imported as @qualified@.
module Hbro.History
    ( Entry(..)
    , InvalidHistoryEntry(..)
    , log
    , log'
    , add
    , add'
    , parseEntry
    , Hbro.History.select
    , Hbro.History.select'
) where

-- {{{ Imports
import Hbro
-- import Hbro.Error
import Hbro.Gui
import Hbro.Misc

import Control.Monad.Reader

import Data.Time

import Network.URI.Monadic

import Prelude hiding(log)

import System.Environment.XDG.BaseDir
import System.IO
import System.Locale
-- }}}

-- {{{ Type definitions
data Entry = Entry
    { _time  :: LocalTime
    , _uri   :: URI
    , _title :: String
    }

instance Show Entry where
    show (Entry time uri title) = unwords [(formatTime defaultTimeLocale dateFormat time), show uri, title]

dateFormat :: String
dateFormat = "%F %T"

data InvalidHistoryEntry = InvalidHistoryEntry String deriving(Typeable)
instance Exception InvalidHistoryEntry
instance Show InvalidHistoryEntry where show (InvalidHistoryEntry x) = "Invalid history entry: " ++ x
-- }}}

-- | Log current visited page to history database
log :: (MonadBase IO m, MonadReader r m, HasGUI r, MonadThrow m) => m ()
log = log' =<< io (getUserDataFile "hbro" "history")

-- | Like 'log', but you can specify the history file path
log' :: (MonadBase IO m, MonadReader r m, HasGUI r, MonadThrow m) => FilePath -> m ()
log' file = do
    uri      <- getCurrentURI
    title    <- getPageTitle
    timeZone <- io $ utcToLocalTime <$> getCurrentTimeZone
    now      <- io $ timeZone <$> getCurrentTime

    add' file (Entry now uri title)

-- | Add a new entry to history database
add :: (MonadBase IO m, MonadThrow m) => Entry -> m ()
add newEntry = (`add'` newEntry) =<< io (getUserDataFile "hbro" "history")

-- | Like 'add', but you can specify the history file path
add' :: (MonadBase IO m, MonadThrow m) => FilePath -> Entry -> m ()
add' file newEntry = do
    io . debugM "hbro.history" $ "Adding new history entry <" ++ show (_uri newEntry) ++ ">"
    -- either (throwError . (show :: IOException -> String)) return =<< (io . try $ withFile file AppendMode (`hPutStrLn` show newEntry))
    io $ withFile file AppendMode (`hPutStrLn` show newEntry)
    --either (\e -> errorHandler file' e >> return False) (const $ return True) result

-- | Try to parse a String into a history Entry.
parseEntry :: (MonadThrow m) => String -> m Entry
parseEntry [] = throwM $ InvalidHistoryEntry []
parseEntry line = (parseEntry' . words) line

parseEntry' :: (MonadThrow m) => [String] -> m Entry
parseEntry' x@(d:t:u:t') = do
    time <- maybe (throwM $ InvalidHistoryEntry (unwords x)) return $ parseTime defaultTimeLocale dateFormat (unwords [d, t])
    uri  <- parseURI u

    return $ Entry time uri (unwords t')
parseEntry' x = throwM $ InvalidHistoryEntry (unwords x)

-- | Open a dmenu with all (sorted alphabetically) history entries, and return the user's selection, if any
select :: (MonadBase IO m, MonadThrow m)
       => [String]          -- ^ dmenu's commandline options
       -> m Entry           -- ^ Selected history entry, if any
select dmenuOptions = (`select'` dmenuOptions) =<< io (getUserDataFile "hbro" "history")

-- | Like 'select', but you can specify the history file path
select' :: (MonadBase IO m, MonadThrow m) => FilePath -> [String] -> m Entry
select' file dmenuOptions = do
    --either (\e -> errorHandler file' e >> return Nothing) (return . return) result
    parseEntry =<< dmenu dmenuOptions . unlines . reverse . sort . nub . lines =<< (io $ readFile file)
