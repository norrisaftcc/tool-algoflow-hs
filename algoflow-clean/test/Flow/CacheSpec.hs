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
import Data.Typeable
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Control.Monad (forM_)

spec :: Spec
spec = describe "Flow.Cache" $ do
  describe "CacheKey" $ do
    it "creates consistent hash for same inputs" $ do
      let key1 = mkCacheKey @Int @String "test" 42 (Proxy @String)
          key2 = mkCacheKey @Int @String "test" 42 (Proxy @String)
      key1 `shouldBe` key2
    
    it "creates different keys for different workflows" $ do
      let key1 = mkCacheKey @Int @String "workflow1" 42 (Proxy @String)
          key2 = mkCacheKey @Int @String "workflow2" 42 (Proxy @String)
      key1 `shouldNotBe` key2
    
    it "creates different keys for different inputs" $ do
      let key1 = mkCacheKey @Int @String "test" 42 (Proxy @String)
          key2 = mkCacheKey @Int @String "test" 43 (Proxy @String)
      key1 `shouldNotBe` key2
    
    it "handles various input types" $ property $ \(x :: Int, y :: String, z :: Bool) ->
      let key1 = mkCacheKey @(Int, String, Bool) @String "multi" (x, y, z) (Proxy @String)
          key2 = mkCacheKey @(Int, String, Bool) @String "multi" (x, y, z) (Proxy @String)
      in key1 == key2

  describe "CacheEntry" $ do
    it "stores value with timestamp" $ do
      now <- getCurrentTime
      let entry = CacheEntry
            { ceData = "test result"
            , ceCreated = now
            , ceHits = 0
            }
      ceData entry `shouldBe` "test result"
      ceHits entry `shouldBe` 0
    
    it "tracks access count" $ do
      now <- getCurrentTime
      let entry = CacheEntry "value" now 5
      ceHits entry `shouldBe` 5

  describe "Cache operations" $ do
    it "stores and retrieves values" $ do
      cache <- inMemoryCache
      let key = mkCacheKey @Int @String "test" 123 (Proxy @String)
      
      -- Initially empty
      result1 <- cacheGet cache key
      result1 `shouldBe` Nothing
      
      -- Store value
      cachePut cache key "result"
      
      -- Retrieve value
      result2 <- cacheGet cache key
      fmap ceData result2 `shouldBe` Just "result"
    
    it "updates access count on get" $ do
      cache <- inMemoryCache
      let key = mkCacheKey @() @Int "counter" () (Proxy @Int)
      
      -- Store initial value
      cachePut cache key (BS8.pack "42")
      
      -- Get multiple times
      Just entry1 <- cacheGet cache key
      ceHits entry1 `shouldBe` 1
      
      Just entry2 <- cacheGet cache key
      ceHits entry2 `shouldBe` 2
    
    it "clears all entries" $ do
      cache <- inMemoryCache
      
      -- Store some entries
      let key1 = mkCacheKey @() @String "clear1" () (Proxy @String)
          key2 = mkCacheKey @() @String "clear2" () (Proxy @String)
          key3 = mkCacheKey @() @String "clear3" () (Proxy @String)
      
      cachePut cache key1 "value1"
      cachePut cache key2 "value2"
      cachePut cache key3 "value3"
      
      -- Verify they exist
      result1 <- cacheGet cache key1
      result1 `shouldNotBe` Nothing
      
      -- Clear cache
      cacheClear cache
      
      -- Verify all gone
      result2 <- cacheGet cache key1
      result3 <- cacheGet cache key2
      result4 <- cacheGet cache key3
      
      result2 `shouldBe` Nothing
      result3 `shouldBe` Nothing
      result4 `shouldBe` Nothing

  describe "Cache integration with withCache" $ do
    it "caches expensive computations" $ do
      cache <- inMemoryCache
      
      -- Simulate expensive computation
      let expensive x = return $ BS8.pack $ show (sum [1..x] :: Int)
          deserialize bs = case reads (BS8.unpack bs) of
                            [(x, "")] -> Just x
                            _ -> Nothing
          serialize = BS8.pack . show
      
      -- First call - compute and store
      result1 <- withCache cache "factorial" (1000 :: Int) deserialize serialize (expensive 1000)
      result1 `shouldBe` 500500
      
      -- Second call - retrieve from cache (same result)
      result2 <- withCache cache "factorial" (1000 :: Int) deserialize serialize (expensive 1000)
      result2 `shouldBe` 500500
    
    it "respects different workflow names" $ do
      cache <- inMemoryCache
      
      let serialize = BS8.pack
          deserialize = Just . BS8.unpack
      
      -- Different workflows, same input
      result1 <- withCache cache "workflow1" (42 :: Int) deserialize serialize (return "result1")
      result2 <- withCache cache "workflow2" (42 :: Int) deserialize serialize (return "result2")
      
      result1 `shouldBe` "result1"
      result2 `shouldBe` "result2"

  describe "Cache statistics" $ do
    it "tracks cache performance" $ do
      cache <- inMemoryCache
      let key = mkCacheKey @() @String "stats" () (Proxy @String)
      
      -- Miss
      miss <- cacheGet cache key
      miss `shouldBe` Nothing
      
      -- Store
      cachePut cache key "cached"
      
      -- Hits
      Just hit1 <- cacheGet cache key
      Just hit2 <- cacheGet cache key
      Just hit3 <- cacheGet cache key
      
      ceHits hit3 `shouldBe` 3
      ceData hit3 `shouldBe` "cached"
      
      -- Check stats
      stats <- cacheStats cache
      length stats `shouldBe` 1
      let (statsKey, hitCount) = head stats
      statsKey `shouldBe` key
      hitCount `shouldBe` 3

  describe "No-op cache" $ do
    it "never stores anything" $ do
      let cache = noOpCache
          key = mkCacheKey @Int @String "test" 42 (Proxy @String)
      
      -- Try to store
      cachePut cache key "value"
      
      -- Should not be found
      result <- cacheGet cache key
      result `shouldBe` Nothing
      
      -- Stats should be empty
      stats <- cacheStats cache
      stats `shouldBe` []