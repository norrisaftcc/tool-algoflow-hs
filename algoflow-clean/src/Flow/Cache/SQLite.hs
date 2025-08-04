{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Flow.Cache.SQLite
Description : SQLite-based persistent cache implementation

This module provides a persistent cache implementation using SQLite.
It implements the same Cache interface as the in-memory cache but
persists data across process restarts.

Schema Design:
- Stores cache entries with metadata (created time, hit count)
- Includes workflow name for targeted invalidation
- Uses compound primary key for uniqueness
- Tracks cache version for migration support
-}

module Flow.Cache.SQLite
  ( -- * SQLite Cache
    sqliteCache
  , SQLiteConfig(..)
  , defaultSQLiteConfig
    
    -- * Migration
  , initializeSchema
  , migrateSchema
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import Control.Exception (bracket, catch, SomeException)
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format (formatTime, parseTimeM, defaultTimeLocale)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Flow.Cache
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

-- | Configuration for SQLite cache
data SQLiteConfig = SQLiteConfig
  { sqlitePath :: FilePath          -- ^ Path to SQLite database file
  , sqliteMaxSize :: Maybe Int64    -- ^ Max cache size in bytes (Nothing = unlimited)
  , sqliteVacuumInterval :: Int     -- ^ How often to vacuum (in operations)
  , sqliteTimeout :: Int            -- ^ Busy timeout in milliseconds
  } deriving (Show, Eq)

-- | Default SQLite configuration
defaultSQLiteConfig :: SQLiteConfig
defaultSQLiteConfig = SQLiteConfig
  { sqlitePath = ".algoflow-cache/cache.db"
  , sqliteMaxSize = Just (100 * 1024 * 1024)  -- 100MB default
  , sqliteVacuumInterval = 1000
  , sqliteTimeout = 5000  -- 5 seconds
  }

-- | Internal representation of a cache row
data CacheRow = CacheRow
  { rowWorkflow :: Text
  , rowInputHash :: Int
  , rowInputType :: Text
  , rowOutputType :: Text
  , rowData :: ByteString
  , rowCreated :: Text  -- ISO8601 format
  , rowHits :: Int
  , rowSize :: Int64
  }

instance FromRow CacheRow where
  fromRow = CacheRow <$> field <*> field <*> field <*> field 
                     <*> field <*> field <*> field <*> field

instance ToRow CacheRow where
  toRow CacheRow{..} = toRow 
    ( rowWorkflow, rowInputHash, rowInputType, rowOutputType
    , rowData, rowCreated, rowHits, rowSize )

-- | Create a SQLite-backed cache
sqliteCache :: MonadIO m => SQLiteConfig -> IO (Cache m)
sqliteCache config = do
  -- Ensure directory exists
  let dbPath = sqlitePath config
  createDirectoryIfMissing True (takeDirectory dbPath)
  
  -- Initialize schema
  withDB dbPath $ \conn -> do
    initializeSchema conn
    -- Set pragmas for performance
    execute_ conn "PRAGMA journal_mode = WAL"
    execute_ conn "PRAGMA synchronous = NORMAL"
    execute_ conn $ Query $ T.pack $ 
      "PRAGMA busy_timeout = " ++ show (sqliteTimeout config)
  
  -- Operation counter for vacuum scheduling
  opsCounter <- newMVar 0
  
  return Cache
    { cacheGet = \key -> liftIO $ withDB dbPath $ \conn -> do
        rows <- query conn 
          "SELECT * FROM cache_entries WHERE workflow = ? AND input_hash = ? \
          \AND input_type = ? AND output_type = ?" 
          (ckWorkflow key, ckInputHash key, show (ckInputType key), show (ckOutputType key))
        
        case rows of
          [] -> return Nothing
          (row:_) -> do
            -- Update hit count
            execute conn
              "UPDATE cache_entries SET hits = hits + 1 \
              \WHERE workflow = ? AND input_hash = ? AND input_type = ? AND output_type = ?"
              (ckWorkflow key, ckInputHash key, show (ckInputType key), show (ckOutputType key))
            
            -- Parse time
            let timeStr = rowCreated row
            case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q" (T.unpack timeStr) of
              Nothing -> return Nothing  -- Corrupted time
              Just created -> return $ Just CacheEntry
                { ceData = rowData row
                , ceCreated = created
                , ceHits = rowHits row + 1
                }
    
    , cachePut = \key value -> liftIO $ do
        now <- getCurrentTime
        let timeStr = T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q" now
            size = fromIntegral $ BS.length value :: Int64
        
        withDB dbPath $ \conn -> do
          -- Insert or replace
          execute conn
            "INSERT OR REPLACE INTO cache_entries \
            \(workflow, input_hash, input_type, output_type, data, created, hits, size) \
            \VALUES (?, ?, ?, ?, ?, ?, 0, ?)"
            ( ckWorkflow key, ckInputHash key, show (ckInputType key)
            , show (ckOutputType key), value, timeStr, size )
          
          -- Check if we need to enforce size limit
          case sqliteMaxSize config of
            Nothing -> return ()
            Just maxSize -> enforceMaxSize conn maxSize
        
        -- Increment operation counter and maybe vacuum
        incrementAndMaybeVacuum dbPath opsCounter (sqliteVacuumInterval config)
    
    , cacheDelete = \key -> liftIO $ withDB dbPath $ \conn ->
        execute conn
          "DELETE FROM cache_entries WHERE workflow = ? AND input_hash = ? \
          \AND input_type = ? AND output_type = ?"
          (ckWorkflow key, ckInputHash key, show (ckInputType key), show (ckOutputType key))
    
    , cacheClear = liftIO $ withDB dbPath $ \conn -> do
        execute_ conn "DELETE FROM cache_entries"
        execute_ conn "VACUUM"
    
    , cacheStats = liftIO $ withDB dbPath $ \conn -> do
        -- TODO: Implement proper TypeRep deserialization
        -- For now, just return empty stats
        return []
    }

-- | Initialize the cache schema
initializeSchema :: Connection -> IO ()
initializeSchema conn = do
  execute_ conn "CREATE TABLE IF NOT EXISTS cache_metadata (\
                \  key TEXT PRIMARY KEY,\
                \  value TEXT NOT NULL\
                \)"
  
  execute_ conn "CREATE TABLE IF NOT EXISTS cache_entries (\
                \  workflow TEXT NOT NULL,\
                \  input_hash INTEGER NOT NULL,\
                \  input_type TEXT NOT NULL,\
                \  output_type TEXT NOT NULL,\
                \  data BLOB NOT NULL,\
                \  created TEXT NOT NULL,\
                \  hits INTEGER NOT NULL DEFAULT 0,\
                \  size INTEGER NOT NULL,\
                \  PRIMARY KEY (workflow, input_hash, input_type, output_type)\
                \)"
  
  -- Create indices for performance
  execute_ conn "CREATE INDEX IF NOT EXISTS idx_cache_workflow ON cache_entries(workflow)"
  execute_ conn "CREATE INDEX IF NOT EXISTS idx_cache_created ON cache_entries(created)"
  execute_ conn "CREATE INDEX IF NOT EXISTS idx_cache_size ON cache_entries(size)"
  
  -- Set schema version
  execute conn "INSERT OR REPLACE INTO cache_metadata (key, value) VALUES (?, ?)"
    ("schema_version" :: Text, "1" :: Text)

-- | Migrate schema if needed
migrateSchema :: Connection -> IO ()
migrateSchema conn = do
  -- Get current version
  versionRows <- query conn "SELECT value FROM cache_metadata WHERE key = ?"
    (Only ("schema_version" :: Text)) :: IO [Only Text]
  
  let currentVersion = case versionRows of
        [Only v] -> read (T.unpack v) :: Int
        _ -> 0
  
  -- Apply migrations based on version
  when (currentVersion < 1) $ do
    -- Future migration code here
    return ()

-- | Enforce maximum cache size by removing oldest entries
enforceMaxSize :: Connection -> Int64 -> IO ()
enforceMaxSize conn maxSize = do
  [Only totalSize] <- query_ conn "SELECT SUM(size) FROM cache_entries"
  
  when (totalSize > maxSize) $ do
    -- Calculate how much to delete
    let targetSize = maxSize * 90 `div` 100  -- Keep 90% after cleanup
        toDelete = totalSize - targetSize
    
    -- Delete oldest entries
    execute conn
      "DELETE FROM cache_entries WHERE rowid IN (\
      \  SELECT rowid FROM cache_entries \
      \  ORDER BY created ASC, hits ASC \
      \  LIMIT (\
      \    SELECT COUNT(*) FROM cache_entries WHERE (\
      \      SELECT SUM(size) FROM cache_entries AS c2 \
      \      WHERE c2.created <= cache_entries.created\
      \    ) <= ?\
      \  )\
      \)"
      (Only toDelete)

-- | Increment operation counter and vacuum if needed
incrementAndMaybeVacuum :: FilePath -> MVar Int -> Int -> IO ()
incrementAndMaybeVacuum dbPath opsCounter interval = do
  shouldVacuum <- modifyMVar opsCounter $ \count -> do
    let newCount = count + 1
    if newCount >= interval
      then return (0, True)
      else return (newCount, False)
  
  when shouldVacuum $ do
    -- Vacuum in a separate connection to avoid blocking
    void $ forkIO $ do
      withDB dbPath $ \conn ->
        execute_ conn "VACUUM" `catch` \(_ :: SomeException) -> return ()

-- | Helper to create a connection
withDB :: FilePath -> (Connection -> IO a) -> IO a
withDB path = bracket (open path) close