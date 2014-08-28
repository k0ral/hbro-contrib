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
import           Hbro
-- import Hbro.Error
import           Hbro.Gui
import           Hbro.Misc

import           Control.Monad.Reader hiding (mapM)

import qualified Data.Set             as Set
-- import Data.Random.Extras
-- import Data.Random.RVar
-- import Data.Random.Source.DevRandom

import           Filesystem           hiding (readFile, writeFile)

import           Network.URI

import           Safe

import           Text.Parsec          hiding (many)
import           Text.Parsec.Text
-- }}}

-- {{{ Type definitions
data Entry = Entry
    { _uri  :: URI
    , _tags :: Set Text
    }

instance Describable Entry where
    describe (Entry uri tags) = unwords $ (tshow uri):(Set.toList tags)
-- }}}

-- Error message
-- invalidBookmarkEntry :: Text -> Text
-- invalidBookmarkEntry = ("Invalid bookmark entry: " ++)

-- | Return bookmarks file
getBookmarksFile :: (BaseIO m) => m FilePath
getBookmarksFile = getAppDataDirectory "hbro" >/> "bookmarks"

-- | Try to parse a Text into a history Entry.
entry :: Parser Entry
entry = do
    spaces
    u <- (some $ satisfy isAllowedInURI) <?> "URI"
    uri <- (maybe mzero return $ parseURI u) <?> "URI"
    some space
    tags <- many $ noneOf "\n"
    return $ Entry uri (Set.fromList . words $ pack tags)


-- | Check if the given bookmark 'Entry' is tagged with the given tag.
hasTag :: Text -> Entry -> Bool
hasTag tag = member tag . _tags

-- | Add current webpage to bookmarks with given tags
add :: (BaseIO m, MonadReader r m, HasGUI r, MonadError Text m, ToSet Text tags) => tags -> m ()
add tags = (`add'` tags) =<< getBookmarksFile

-- | Like 'add', but you can specify the bookmarks file path
add' :: (BaseIO m, MonadReader r m, Hbro.Gui.HasGUI r, MonadError Text m, ToSet Text tags) => FilePath -> tags -> m ()
add' file tags = do
    uri <- getCurrentURI
    void . addCustom file $ Entry uri (toSet tags)

-- | Add a custom entry to bookmarks
addCustom :: (BaseIO m, MonadError Text m)
          => FilePath      -- ^ Bookmarks file
          -> Entry         -- ^ Custom bookmark entry
          -> m ()
addCustom file newEntry = io $ withFile file AppendMode (`hPutStrLn` describe newEntry)

-- | Open a dmenu with all (sorted alphabetically) bookmarks entries, and return the user's selection, if any.
select :: (ControlIO m, MonadError Text m)
       => [Text]         -- ^ dmenu's commandline options
       -> m URI
select dmenuOptions = (`select'` dmenuOptions) =<< getBookmarksFile

-- | Like 'select', but you can specify the bookmarks file path
select' :: (ControlIO m, MonadError Text m) => FilePath -> [Text] -> m URI
select' file dmenuOptions = do
    maybe (throwError "ERROR") return . parseURIReference . unpack . lastDef "" . words =<< dmenu dmenuOptions . unlines . sort . ordNub . (map reformat) . lines =<< readFile file

reformat :: Text -> Text
reformat line = unwords $ tags' ++ [uri]
  where
    uri:tags = words line
    tags'    = sort $ map (("[" ++) . (++ "]")) tags

-- | Open a dmenu with all (sorted alphabetically) bookmarks tags, and return the user's selection, if any.
selectByTag :: (ControlIO m, MonadError Text m)
          => [Text]          -- ^ dmenu's commandline options
          -> m [URI]
selectByTag dmenuOptions = (`selectByTag'` dmenuOptions) =<< getBookmarksFile

-- | Like 'selectByTag', but you can specify the bookmarks file path
selectByTag' :: (ControlIO m, MonadError Text m)
          => FilePath          -- ^ Bookmarks' database file
          -> [Text]            -- ^ dmenu's commandline options
          -> m [URI]
selectByTag' file dmenuOptions = do
-- Read bookmarks file
    result <- readFile file

    entries <- mapM (either (throwError . tshow) return . runParser entry () "(unknown)") . lines $ result
    let tags = unlines . Set.toList . foldl' Set.union Set.empty $ map _tags entries

-- Let user select a tag
    (map _uri) . (\t -> filter (hasTag t) entries) <$> dmenu dmenuOptions tags

--
--popOldest :: PortableFilePath -> Text -> IO (Maybe URI)
--popOldest file tags = do

-- Return a random Bookmark entry with a given tag, while removing it from bookmarks.
-- popRandom :: PortableFilePath
--           -> Text
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
--             writeFile file' . unlines . ordNub $ newLines

--             return . parseURIReference . last . words $ value


-- | Remove all bookmarks entries matching the given tag.
deleteByTag :: (ControlIO m, MonadError Text m)
          => [Text]          -- ^ dmenu's commandline options
          -> m ()
deleteByTag dmenuOptions = (`deleteByTag'` dmenuOptions) =<< getBookmarksFile

-- | Like 'selectByTag', but you can specify the bookmarks file path
deleteByTag' :: (ControlIO m, MonadError Text m)
              => FilePath          -- ^ Bookmarks' database file
              -> [Text]            -- ^ dmenu's commandline options
              -> m ()
deleteByTag' file dmenuOptions = do
    result <- readFile file

    entries <- mapM (either (throwError . tshow) return . runParser entry () "(unknown)") . lines $ result
    let tags = unlines . Set.toList . (foldl' Set.union Set.empty) $ map _tags entries

    tag <- dmenu dmenuOptions tags
    writeFile (file <.> "old") $ unlines (describe <$> entries)
    writeFile file $ (unlines . map describe . (filter (not . (hasTag tag)))) entries
