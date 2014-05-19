-- | Simple bookmarks system: bookmarks are a list of tagged URIs.
--
-- This module is designed to be imported as @qualified@.
module Hbro.Bookmarks
    ( Entry(..)
    , add
    , add'
    , addCustom
    , Hbro.Bookmarks.select
    , Hbro.Bookmarks.select'
    , selectByTag
    , deleteByTag
) where

-- {{{ Imports
import Hbro
-- import Hbro.Error
import Hbro.Gui
import Hbro.Misc

import Control.Monad.Reader

-- import Data.Random.Extras
-- import Data.Random.RVar
-- import Data.Random.Source.DevRandom

import Network.URI.Monadic

import Prelude hiding(mapM_)

import System.Environment.XDG.BaseDir
import System.IO
-- }}}

-- {{{ Type definitions
data Entry = Entry
    { _uri  :: URI
    , _tags :: [String]
    }

instance Show Entry where
    show (Entry uri tags) = unwords $ (show uri):tags

data InvalidBookmarkEntry = InvalidBookmarkEntry String deriving(Typeable)
instance Exception InvalidBookmarkEntry
instance Show InvalidBookmarkEntry where show (InvalidBookmarkEntry x) = "Invalid bookmark entry: " ++ x
-- }}}

-- | Try to parse a String into a bookmark Entry.
parseEntry :: (MonadThrow m) => String -> m Entry
parseEntry [] = throwM $ InvalidBookmarkEntry []
parseEntry line = return (words line)
    >>= (\(h:t) -> parseURI h
    >>= (\uri -> return $ Entry uri t))

-- | Check if the given bookmark 'Entry' is tagged with the given tag.
hasTag :: String -> Entry -> Bool
hasTag tag = isJust . (find $ (==) tag) . _tags

-- | Add current webpage to bookmarks with given tags
add :: (MonadBase IO m, MonadReader r m, HasGUI r, MonadThrow m) => [String] -> m ()
add tags = (`add'` tags) =<< io (getUserDataFile "hbro" "bookmarks")

-- | Like 'add', but you can specify the bookmarks file path
add' :: (MonadBase IO m, MonadReader r m, Hbro.Gui.HasGUI r, MonadThrow m) => FilePath -> [String] -> m ()
add' file tags = do
    uri <- getCurrentURI
    void . addCustom file $ Entry uri tags

-- | Add a custom entry to bookmarks
addCustom :: (MonadBase IO m, MonadThrow m)
          => FilePath      -- ^ Bookmarks file
          -> Entry         -- ^ Custom bookmark entry
          -> m ()
addCustom file newEntry = io $ withFile file AppendMode (`hPutStrLn` show newEntry)

-- | Open a dmenu with all (sorted alphabetically) bookmarks entries, and return the user's selection, if any.
select :: (MonadBase IO m, MonadThrow m)
       => [String]         -- ^ dmenu's commandline options
       -> m URI
select dmenuOptions = (`select'` dmenuOptions) =<< io (getUserDataFile "hbro" "bookmarks")

-- | Like 'select', but you can specify the bookmarks file path
select' :: (MonadBase IO m, MonadThrow m) => FilePath -> [String] -> m URI
select' file dmenuOptions = do
    result <- io $ readFile file
    parseURIReference . last . words =<< (dmenu dmenuOptions . unlines . sort . nub . (map reformat) . lines $ result)

reformat :: String -> String
reformat line = unwords $ tags' ++ [uri]
  where
    uri:tags = words line
    tags'    = sort $ map (\tag -> '[':(tag ++ "]")) tags

-- | Open a dmenu with all (sorted alphabetically) bookmarks tags, and return the user's selection, if any.
selectByTag :: (MonadBase IO m, MonadThrow m)
          => [String]          -- ^ dmenu's commandline options
          -> m [URI]
selectByTag dmenuOptions = (`selectByTag'` dmenuOptions) =<< io (getUserDataFile "hbro" "bookmarks")

-- | Like 'selectByTag', but you can specify the bookmarks file path
selectByTag' :: (MonadBase IO m, MonadThrow m)
          => FilePath          -- ^ Bookmarks' database file
          -> [String]          -- ^ dmenu's commandline options
          -> m [URI]
selectByTag' file dmenuOptions = do
-- Read bookmarks file
    result <- (io $ readFile file)

    entries <- mapM parseEntry . lines $ result
    let tags = unlines . sort . nub . words . unwords . foldr (union . _tags) [] $ entries

-- Let user select a tag
    (map _uri) . (\t -> filter (hasTag t) entries) <$> dmenu dmenuOptions tags

--
--popOldest :: PortableFilePath -> String -> IO (Maybe URI)
--popOldest file tags = do

-- Return a random Bookmark entry with a given tag, while removing it from bookmarks.
-- popRandom :: PortableFilePath
--           -> String
--           -> IO (Maybe URI)
-- popRandom file tags = do
--     file'  <- resolve file
--     result <- try . readFile $ file'
--     file'' <- either (\e -> errorHandler file' e >> return Nothing) (\x -> return $ Just x) result

--     forM_ file'' $ \f -> do
--         let selection = choiceExtract . lines $ f
--         forM_ selection $ \s -> do
--             (newLines, value) <- runRVar s DevURandom

--             renameFile file' (file' ++ ".old")
--             writeFile file' . unlines . nub $ newLines

--             return . parseURIReference . last . words $ value


-- | Remove all bookmarks entries matching the given tag.
deleteByTag :: (MonadBase IO m, MonadThrow m)
          => [String]          -- ^ dmenu's commandline options
          -> m ()
deleteByTag dmenuOptions = (`deleteByTag'` dmenuOptions) =<< io (getUserDataFile "hbro" "bookmarks")

-- | Like 'selectByTag', but you can specify the bookmarks file path
deleteByTag' :: (Functor m, MonadBase IO m, MonadThrow m)
              => FilePath          -- ^ Bookmarks' database file
              -> [String]          -- ^ dmenu's commandline options
              -> m ()
deleteByTag' file dmenuOptions = do
    result <- (io $ readFile file)

    entries <- mapM parseEntry . lines $ result
    let tags = (unlines . sort . nub . words . unwords . (foldr (union . _tags) [])) entries

    tag <- dmenu dmenuOptions tags
    io $ writeFile (file ++ ".old") $ unlines (map show entries)
    io $ writeFile file $ (unlines . (map show) . (filter (not . (hasTag tag)))) entries
