{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | Simple bookmarks system: bookmarks are a list of tagged URIs.
--
-- This module is designed to be imported as @qualified@.
module Hbro.Bookmarks
    ( Entry(..)
    , addCurrent
    , addCurrent'
    , add
    , Hbro.Bookmarks.select
    , Hbro.Bookmarks.select'
    , selectByTag
    , deleteByTag
    ) where

-- {{{ Imports
import           Hbro
import           Hbro.Logger
import           Hbro.Misc

import           Data.Aeson.Extended
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Set             as Set
-- import Data.Random.Extras
-- import Data.Random.RVar
-- import Data.Random.Source.DevRandom

import           Filesystem           hiding (readFile, writeFile)

import           Network.URI.Extended

import           Safe
-- }}}

-- {{{ Type definitions
data Entry = Entry
    { _uri  :: URI
    , _tags :: Set Text
    }

deriving instance Eq Entry
deriving instance Ord Entry

instance Describable Entry where
    describe (Entry uri tags) = unwords $ map (\x -> "[" ++ x ++ "]") (Set.toList tags) ++ [tshow uri]

instance FromJSON Entry where
    parseJSON (Object v) = Entry <$> v .: "uri" <*> v .: "tags"
    parseJSON _          = mzero

instance ToJSON Entry where
    toJSON (Entry u t) = object ["uri" .= u, "tags" .= t]
-- }}}

-- {{{ Util
readFileE :: (ControlIO m, MonadError Text m) => FilePath -> m Lazy.ByteString
readFileE = handleIO (throwError . tshow) . readFile

writeFileE :: (ControlIO m, MonadError Text m) => FilePath -> Lazy.ByteString -> m ()
writeFileE f x = handleIO (throwError . tshow) $ writeFile f x
-- }}}

-- | Return bookmarks file
getBookmarksFile :: (BaseIO m) => m FilePath
getBookmarksFile = getAppDataDirectory "hbro" >/> "bookmarks"

-- | Check if the given bookmark 'Entry' is tagged with the given tag.
hasTag :: Text -> Entry -> Bool
hasTag tag = member tag . _tags

-- | Add current webpage to bookmarks with given tags
addCurrent :: (ControlIO m, MonadLogger m, MonadReader r m, Has MainView r, MonadError Text m, Alternative m) => [Text] -> m ()
addCurrent tags = (`addCurrent'` tags) =<< getBookmarksFile

-- | Like 'add', but you can specify the bookmarks file path
addCurrent' :: (ControlIO m, MonadLogger m, MonadReader r m, Has MainView r, MonadError Text m, Alternative m) => FilePath -> [Text] -> m ()
addCurrent' file tags = do
    uri <- getCurrentURI
    void . add file $ Entry uri (Set.fromList tags)

-- | Add a custom entry to bookmarks
add :: (ControlIO m, MonadLogger m, MonadError Text m, Alternative m)
    => FilePath      -- ^ Bookmarks file
    -> Entry         -- ^ Custom bookmark entry
    -> m ()
add file newEntry = do
  info $ "New bookmark: " ++ describe newEntry
  tryIO . io . copyFile file $ file <.> "bak"
  entries <- (decodeM =<< readFileE file) <|> return Set.empty
  writeFileE file . encodePretty $ Set.insert newEntry entries

-- | Open a dmenu with all (sorted alphabetically) bookmarks entries, and return the user's selection, if any.
select :: (ControlIO m, MonadError Text m) => m URI
select = (`select'` defaultDmenuOptions) =<< getBookmarksFile

-- | Like 'select', but you can specify the bookmarks file path
select' :: (ControlIO m, MonadError Text m) => FilePath -> [Text] -> m URI
select' file dmenuOptions = parseURIReferenceM . lastDef "" . words =<< dmenu dmenuOptions . unlines . sort . map (describe :: Entry -> Text) . Set.toList =<< decodeM =<< readFileE file

-- | Open a dmenu with all (sorted alphabetically) bookmarks tags, and return the user's selection, if any.
selectByTag :: (ControlIO m, MonadLogger m, MonadError Text m) => m [URI]
selectByTag = (`selectByTag'` defaultDmenuOptions) =<< getBookmarksFile

-- | Like 'selectByTag', but you can specify the bookmarks file path
selectByTag' :: (ControlIO m, MonadLogger m, MonadError Text m)
          => FilePath          -- ^ Bookmarks' database file
          -> [Text]            -- ^ dmenu's commandline options
          -> m [URI]
selectByTag' file dmenuOptions = do
-- Read bookmarks file
    entries <- decodeM =<< readFileE file
    let tags = unlines . Set.toList . foldl' Set.union Set.empty $ map _tags entries

-- Let user select a tag
    tag <- dmenu dmenuOptions tags
    debug $ "User selected tag " ++ tag
    return . map _uri $ filter (hasTag tag) entries

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
deleteByTag :: (ControlIO m, MonadLogger m, MonadError Text m) => m ()
deleteByTag = (`deleteByTag'` defaultDmenuOptions) =<< getBookmarksFile

-- | Like 'selectByTag', but you can specify the bookmarks file path
deleteByTag' :: (ControlIO m, MonadLogger m, MonadError Text m)
              => FilePath          -- ^ Bookmarks' database file
              -> [Text]            -- ^ dmenu's commandline options
              -> m ()
deleteByTag' file dmenuOptions = do
    entries <- decodeM =<< readFileE file
    let tags = unlines . Set.toList . foldl' Set.union Set.empty . map _tags $ Set.toList entries

    tag <- dmenu dmenuOptions tags
    info $ "Deleting bookmarks with tag " ++ tag
    tryIO . io . copyFile file $ file <.> "bak"
    writeFileE file . encodePretty $ Set.filter (not . hasTag tag) entries
