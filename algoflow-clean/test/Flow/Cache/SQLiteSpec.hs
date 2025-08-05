{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Flow.Cache.SQLiteSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM_)
import Control.Exception (bracket)
import Control.Concurrent.Async (forConcurrently)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Word (Word8)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Proxy (Proxy(..))
import Data.IORef
import Data.Maybe (isJust)
import System.Directory (removeFile, doesFileExist)
import System.IO.Temp (withSystemTempDirectory)
import Flow.Cache
import Flow.Cache.SQLite
import Data.Time (getCurrentTime, diffUTCTime)

spec :: Spec
spec = do
  describe "SQLite Cache" $ do
    describe "Basic Operations" $ do
      it "should store and retrieve values" $ 
        withTempCache $ \cache -> do
          let key = mkCacheKey @Int @String "test" 42 (Proxy @String)
              value = "hello world"
              encoded = BS.pack value
          
          cachePut cache key encoded
          result <- cacheGet cache key
          
          case result of
            Nothing -> expectationFailure "Failed to retrieve cached value"
            Just entry -> ceData entry `shouldBe` encoded
      
      it "should return Nothing for missing keys" $
        withTempCache $ \cache -> do
          let key = mkCacheKey @Int @String "missing" 999 (Proxy @String)
          result <- cacheGet cache key
          result `shouldBe` Nothing
      
      it "should increment hit count on retrieval" $
        withTempCache $ \cache -> do
          let key = mkCacheKey @Int @Int "hits" 123 (Proxy @Int)
              value = BS.pack "test"
          
          cachePut cache key value
          
          -- First retrieval
          Just entry1 <- cacheGet cache key
          ceHits entry1 `shouldBe` 1
          
          -- Second retrieval
          Just entry2 <- cacheGet cache key
          ceHits entry2 `shouldBe` 2
      
      it "should delete specific entries" $
        withTempCache $ \cache -> do
          let key = mkCacheKey @Int @String "delete" 42 (Proxy @String)
              value = BS.pack "delete me"
          
          cachePut cache key value
          Just _ <- cacheGet cache key
          
          cacheDelete cache key
          result <- cacheGet cache key
          result `shouldBe` Nothing
      
      it "should clear all entries" $
        withTempCache $ \cache -> do
          let key1 = mkCacheKey @Int @String "clear1" 1 (Proxy @String)
              key2 = mkCacheKey @Int @String "clear2" 2 (Proxy @String)
          
          cachePut cache key1 (BS.pack "value1")
          cachePut cache key2 (BS.pack "value2")
          
          cacheClear cache
          
          result1 <- cacheGet cache key1
          result2 <- cacheGet cache key2
          
          result1 `shouldBe` Nothing
          result2 `shouldBe` Nothing
    
    describe "Persistence" $ do
      it "should persist data across cache instances" $ 
        withSystemTempDirectory "algoflow-test" $ \tmpDir -> do
          let config = defaultSQLiteConfig { sqlitePath = tmpDir ++ "/test.db" }
          
          -- First instance: store data
          cache1 <- sqliteCache config
          let key = mkCacheKey @Int @String "persist" 42 (Proxy @String)
              value = BS.pack "persistent"
          cachePut cache1 key value
          
          -- Second instance: retrieve data
          cache2 <- sqliteCache config
          result <- cacheGet cache2 key
          
          case result of
            Nothing -> expectationFailure "Data not persisted"
            Just entry -> ceData entry `shouldBe` value
    
    describe "Size Management" $ do
      it "should enforce max size limit" $ 
        withSystemTempDirectory "algoflow-test" $ \tmpDir -> do
          let config = defaultSQLiteConfig 
                { sqlitePath = tmpDir ++ "/size.db"
                , sqliteMaxSize = Just 1024  -- 1KB limit
                }
          cache <- sqliteCache config
          
          -- Fill cache with large entries
          forM_ [1..10] $ \i -> do
            let key = mkCacheKey @Int @ByteString "size" i (Proxy @ByteString)
                value = BS.pack $ replicate 200 (toEnum i)  -- 200 bytes each
            cachePut cache key value
          
          -- Older entries should be evicted
          result1 <- cacheGet cache (mkCacheKey @Int @ByteString "size" 1 (Proxy @ByteString))
          result1 `shouldBe` Nothing  -- Should be evicted
          
          -- Newer entries should remain
          result10 <- cacheGet cache (mkCacheKey @Int @ByteString "size" 10 (Proxy @ByteString))
          result10 `shouldNotBe` Nothing
    
    describe "Concurrent Access" $ do
      it "should handle concurrent operations safely" $
        withTempCache $ \cache -> do
          let key = mkCacheKey @Int @Int "concurrent" 42 (Proxy @Int)
              value = BS.pack "concurrent"
          
          -- Spawn multiple concurrent operations
          results <- forConcurrently [1..10] $ \i -> do
            cachePut cache key value
            cacheGet cache key
          
          -- All operations should succeed
          all isJust results `shouldBe` True
    
    describe "withCache helper" $ do
      it "should compute and cache on miss" $
        withTempCache $ \cache -> do
          let workflow = "compute"
              input = 100 :: Int
              serialize = BS.pack . show
              deserialize bs = case reads (BS.unpack bs) of
                [(x, "")] -> Just x
                _ -> Nothing
              compute = return (input * 2) :: IO Int
          
          result <- withCache cache workflow input deserialize serialize compute
          result `shouldBe` 200
          
          -- Second call should use cache
          callCount <- newIORef (0 :: Int)
          result2 <- withCache cache workflow input deserialize serialize $ do
            modifyIORef' callCount (+1)
            compute
          
          result2 `shouldBe` 200
          count <- readIORef callCount
          count `shouldBe` 0  -- Computation not called
      
      it "should recompute on cache corruption" $
        withTempCache $ \cache -> do
          let workflow = "corrupt"
              input = 50 :: Int
              serialize = const (BS.pack "corrupted")
              deserialize = const Nothing  -- Always fail deserialization
              compute = return (input + 1) :: IO Int
          
          -- First call
          result1 <- withCache cache workflow input deserialize serialize compute
          result1 `shouldBe` 51
          
          -- Second call should recompute due to corruption
          result2 <- withCache cache workflow input deserialize serialize compute
          result2 `shouldBe` 51
    
    describe "Schema Migration" $ do
      it "should initialize schema on first use" $
        withSystemTempDirectory "algoflow-test" $ \tmpDir -> do
          let config = defaultSQLiteConfig { sqlitePath = tmpDir ++ "/schema.db" }
          
          -- Create cache (initializes schema)
          _ :: Cache IO <- sqliteCache config
          
          -- Check file exists
          exists <- doesFileExist (sqlitePath config)
          exists `shouldBe` True
      
      it "should handle schema versioning" $
        withSystemTempDirectory "algoflow-test" $ \tmpDir -> do
          let dbPath = tmpDir ++ "/version.db"
              config = defaultSQLiteConfig { sqlitePath = dbPath }
          
          -- Create initial cache
          cache1 <- sqliteCache config
          let key = mkCacheKey @Int @String "version" 1 (Proxy @String)
          cachePut cache1 key (BS.pack "v1")
          
          -- Future: Test migration when schema changes
          -- For now, just verify it doesn't crash
          cache2 <- sqliteCache config
          result <- cacheGet cache2 key
          result `shouldNotBe` Nothing

-- Helper functions

withTempCache :: (Cache IO -> IO a) -> IO a
withTempCache action = 
  withSystemTempDirectory "algoflow-test" $ \tmpDir -> do
    let config = defaultSQLiteConfig { sqlitePath = tmpDir ++ "/test.db" }
    cache <- sqliteCache config
    action cache

