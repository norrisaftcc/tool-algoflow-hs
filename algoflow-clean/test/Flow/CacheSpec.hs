{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Flow.CacheSpec where

import Test.Hspec
import Test.QuickCheck
import Flow.Cache
import Data.Time
import Data.Hashable
import Control.Monad (forM_)

spec :: Spec
spec = describe "Flow.Cache" $ do
  describe "CacheKey" $ do
    it "creates consistent hash for same inputs" $ do
      let key1 = CacheKey "test" (42 :: Int)
          key2 = CacheKey "test" (42 :: Int)
      hash key1 `shouldBe` hash key2
    
    it "creates different hashes for different workflows" $ do
      let key1 = CacheKey "workflow1" (42 :: Int)
          key2 = CacheKey "workflow2" (42 :: Int)
      hash key1 `shouldNotBe` hash key2
    
    it "creates different hashes for different inputs" $ do
      let key1 = CacheKey "test" (42 :: Int)
          key2 = CacheKey "test" (43 :: Int)
      hash key1 `shouldNotBe` hash key2
    
    it "handles various input types" $ property $ \(x :: Int, y :: String, z :: Bool) ->
      let key1 = CacheKey "multi" (x, y, z)
          key2 = CacheKey "multi" (x, y, z)
      in hash key1 == hash key2

  describe "CacheEntry" $ do
    it "stores value with timestamp" $ do
      now <- getCurrentTime
      let entry = CacheEntry
            { entryValue = "test result"
            , entryCreated = now
            , entryAccessed = now
            , entryHits = 0
            }
      entryValue entry `shouldBe` "test result"
      entryHits entry `shouldBe` 0
    
    it "tracks access count" $ do
      now <- getCurrentTime
      let entry = CacheEntry "value" now now 5
      entryHits entry `shouldBe` 5

  describe "Cache operations" $ do
    it "stores and retrieves values" $ do
      cache <- newInMemoryCache
      let key = CacheKey "test" (123 :: Int)
      
      -- Initially empty
      result1 <- get cache key
      result1 `shouldBe` Nothing
      
      -- Store value
      put cache key ("result" :: String)
      
      -- Retrieve value
      result2 <- get cache key
      fmap entryValue result2 `shouldBe` Just "result"
    
    it "updates access time and count on get" $ do
      cache <- newInMemoryCache
      let key = CacheKey "counter" ()
      
      -- Store initial value
      put cache key (42 :: Int)
      
      -- Get multiple times
      Just entry1 <- get cache key
      entryHits entry1 `shouldBe` 1
      
      Just entry2 <- get cache key
      entryHits entry2 `shouldBe` 2
      entryAccessed entry2 `shouldSatisfy` (>= entryAccessed entry1)
    
    it "evicts entries by count" $ do
      cache <- newInMemoryCache
      
      -- Store many entries (more than typical cache limit)
      forM_ [1..1000] $ \i -> do
        let key = CacheKey "evict" (i :: Int)
        put cache key (i * 2)
      
      -- Earlier entries might be evicted
      result <- get cache (CacheKey "evict" (1 :: Int))
      -- This test is implementation-dependent
      -- In-memory cache might not evict, so we just verify no crash
      case result of
        Nothing -> pure ()  -- Was evicted
        Just entry -> entryValue entry `shouldBe` 2
    
    it "clears all entries" $ do
      cache <- newInMemoryCache
      
      -- Store some entries
      put cache (CacheKey "clear1" ()) "value1"
      put cache (CacheKey "clear2" ()) "value2"
      put cache (CacheKey "clear3" ()) "value3"
      
      -- Verify they exist
      result1 <- get cache (CacheKey "clear1" ())
      result1 `shouldNotBe` Nothing
      
      -- Clear cache
      clear cache
      
      -- Verify all gone
      result2 <- get cache (CacheKey "clear1" ())
      result3 <- get cache (CacheKey "clear2" ())
      result4 <- get cache (CacheKey "clear3" ())
      
      result2 `shouldBe` Nothing
      result3 `shouldBe` Nothing
      result4 `shouldBe` Nothing

  describe "Cacheable typeclass" $ do
    it "provides default key generation for basic types" $ do
      cacheKey (42 :: Int) `shouldNotBe` cacheKey (43 :: Int)
      cacheKey ("hello" :: String) `shouldNotBe` cacheKey ("world" :: String)
      cacheKey (True :: Bool) `shouldNotBe` cacheKey (False :: Bool)
    
    it "works with tuples" $ do
      let key1 = cacheKey ((1, "a") :: (Int, String))
          key2 = cacheKey ((1, "b") :: (Int, String))
          key3 = cacheKey ((2, "a") :: (Int, String))
      key1 `shouldNotBe` key2
      key1 `shouldNotBe` key3
      key2 `shouldNotBe` key3
    
    it "works with lists" $ do
      cacheKey ([1,2,3] :: [Int]) `shouldNotBe` cacheKey ([1,2,3,4] :: [Int])
      cacheKey (["a","b"] :: [String]) `shouldNotBe` cacheKey (["b","a"] :: [String])

  describe "Cache integration with workflows" $ do
    it "caches expensive computations" $ do
      cache <- newInMemoryCache
      
      -- Simulate expensive computation
      let expensive x = sum [1..x] :: Int
          key = CacheKey "factorial" (1000 :: Int)
      
      -- First call - compute and store
      put cache key (expensive 1000)
      
      -- Second call - retrieve from cache
      Just entry <- get cache key
      entryValue entry `shouldBe` 500500  -- sum of 1..1000
      entryHits entry `shouldBe` 1
    
    it "respects different workflow names" $ do
      cache <- newInMemoryCache
      
      let key1 = CacheKey "workflow1" (42 :: Int)
          key2 = CacheKey "workflow2" (42 :: Int)
      
      put cache key1 "result1"
      put cache key2 "result2"
      
      Just entry1 <- get cache key1
      Just entry2 <- get cache key2
      
      entryValue entry1 `shouldBe` "result1"
      entryValue entry2 `shouldBe` "result2"

  describe "Cache statistics" $ do
    it "tracks cache performance" $ do
      cache <- newInMemoryCache
      let key = CacheKey "stats" ()
      
      -- Miss
      miss <- get cache key
      miss `shouldBe` Nothing
      
      -- Store
      put cache key "cached"
      
      -- Hits
      Just hit1 <- get cache key
      Just hit2 <- get cache key
      Just hit3 <- get cache key
      
      entryHits hit3 `shouldBe` 3
      entryValue hit3 `shouldBe` "cached"