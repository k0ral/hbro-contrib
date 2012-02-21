module Hbro.History (
    Entry(..),
    log,
    add,
    parseEntry,
    select
) where

-- {{{ Imports
import Hbro.Core
import Hbro.Types
import Hbro.Util

import Control.Exception
--import Control.Monad.Reader

import Data.Functor
import Data.List
import Data.Time

import Network.URI

import Prelude hiding(log)

--import System.IO.Error
import System.IO
import System.Locale
-- }}} 

-- {{{ Type definitions
data Entry = Entry {
    mTime  :: LocalTime,
    mURI   :: URI, 
    mTitle :: String
}

instance Show Entry where
    show (Entry time uri title) = unwords [(formatTime defaultTimeLocale dateFormat time), show uri, title]

dateFormat :: String
dateFormat = "%F %T"
-- }}}

-- | Log current visited page to history file
log :: PortableFilePath -> K ()
log file = withURI $ \uri -> withTitle $ \title -> io $ do
    timeZone <- utcToLocalTime <$> getCurrentTimeZone
    now      <- timeZone <$> getCurrentTime
    
    add file (Entry now uri title) >> return ()

-- | Add a new entry to history file
add :: PortableFilePath  -- ^ History file
    -> Entry             -- ^ History entry to add
    -> IO Bool
add file newEntry = do
    file'  <- resolve file
    result <- try $ withFile file' AppendMode (`hPutStrLn` show newEntry)
    either (\e -> errorHandler file' e >> return False) (const $ return True) result    

-- | Try to parse a String into a history Entry.
parseEntry :: String -> Maybe Entry
parseEntry [] = Nothing
parseEntry line = (parseEntry' . words) line

parseEntry' :: [String] -> Maybe Entry
parseEntry' (d:t:u:t') = do
    time <- parseTime defaultTimeLocale dateFormat (unwords [d, t])
    uri  <- parseURI u
    
    return $ Entry time uri (unwords t')
parseEntry' _ = Nothing

-- | Open a dmenu with all (sorted alphabetically) history entries, and return the user's selection, if any
select :: PortableFilePath  -- ^ Path to history file
       -> [String]          -- ^ dmenu's commandline options
       -> IO (Maybe Entry)  -- ^ Selected history entry, if any
select file dmenuOptions = do
    file'  <- resolve file
    result <- try $ readFile file'
        
    either (\e -> errorHandler file' e >> return Nothing) (return . return) result
    >>= (return . ((return . unlines . reverse . sort . nub . lines) =<<))
    >>= (maybe (return Nothing) (dmenu dmenuOptions))
    >>= (return . (parseEntry =<<))


reformat :: String -> String
reformat line = 
  let
        _date:_time:uri:title = words line 
  in 
        unwords $ [uri] ++ title
    
