{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Flow.Cache
  ( -- * Cache Interface
    Cache(..)
  , CacheKey
  , CacheEntry(..)
    
    -- * Cache Implementations
  , inMemoryCache
  , noOpCache
    
    -- * Cache Operations
  , withCache
  , invalidateCache
  ) where

import Control.Concurrent.MVar
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Hashable (Hashable, hash)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import Data.Typeable (Typeable, TypeRep, typeRep, Proxy(..))
import GHC.Generics (Generic)

-- | A cache key combines the workflow name, input hash, and type information
data CacheKey = CacheKey
  { ckWorkflow :: !Text
  , ckInputHash :: !Int
  , ckInputType :: !TypeRep
  , ckOutputType :: !TypeRep
  } deriving (Eq, Ord, Show, Generic)

-- | A cache entry stores the result and metadata
data CacheEntry = CacheEntry
  { ceData :: !ByteString
  , ceCreated :: !UTCTime
  , ceHits :: !Int
  } deriving (Show, Generic)

-- | Abstract cache interface
data Cache m = Cache
  { cacheGet :: CacheKey -> m (Maybe CacheEntry)
  , cachePut :: CacheKey -> ByteString -> m ()
  , cacheDelete :: CacheKey -> m ()
  , cacheClear :: m ()
  , cacheStats :: m [(CacheKey, Int)] -- Returns hit counts
  }

-- | Create an in-memory cache using an MVar
inMemoryCache :: MonadIO m => IO (Cache m)
inMemoryCache = do
  cacheVar <- newMVar Map.empty
  return Cache
    { cacheGet = \key -> liftIO $ do
        cache <- readMVar cacheVar
        case Map.lookup key cache of
          Nothing -> return Nothing
          Just entry -> do
            -- Update hit count
            modifyMVar_ cacheVar $ \c ->
              return $ Map.adjust (\e -> e { ceHits = ceHits e + 1 }) key c
            return (Just entry)
    
    , cachePut = \key value -> liftIO $ do
        now <- getCurrentTime
        let entry = CacheEntry value now 0
        modifyMVar_ cacheVar $ \cache ->
          return $ Map.insert key entry cache
    
    , cacheDelete = \key -> liftIO $ do
        modifyMVar_ cacheVar $ \cache ->
          return $ Map.delete key cache
    
    , cacheClear = liftIO $ do
        modifyMVar_ cacheVar $ \_ -> return Map.empty
    
    , cacheStats = liftIO $ do
        cache <- readMVar cacheVar
        return [(k, ceHits v) | (k, v) <- Map.toList cache]
    }

-- | A no-op cache that never stores anything
noOpCache :: Monad m => Cache m
noOpCache = Cache
  { cacheGet = \_ -> return Nothing
  , cachePut = \_ _ -> return ()
  , cacheDelete = \_ -> return ()
  , cacheClear = return ()
  , cacheStats = return []
  }

-- | Helper to create a cache key
mkCacheKey :: (Typeable a, Typeable b, Hashable a) 
           => Text -> a -> Proxy b -> CacheKey
mkCacheKey workflow input outputProxy = CacheKey
  { ckWorkflow = workflow
  , ckInputHash = hash input
  , ckInputType = typeRep (Proxy :: Proxy a)
  , ckOutputType = typeRep outputProxy
  }

-- | Use cache for a computation
withCache :: (MonadIO m, Typeable a, Typeable b, Hashable a)
          => Cache m
          -> Text                    -- ^ Workflow name
          -> a                       -- ^ Input
          -> (ByteString -> Maybe b) -- ^ Deserializer
          -> (b -> ByteString)       -- ^ Serializer
          -> m b                     -- ^ Computation
          -> m b
withCache cache workflow input deserialize serialize compute = do
  let key = mkCacheKey workflow input (Proxy :: Proxy b)
  
  -- Try to get from cache
  cached <- cacheGet cache key
  case cached of
    Just entry -> 
      case deserialize (ceData entry) of
        Just value -> return value
        Nothing -> do
          -- Cache corruption, recompute
          result <- compute
          cachePut cache key (serialize result)
          return result
    
    Nothing -> do
      -- Not in cache, compute and store
      result <- compute
      cachePut cache key (serialize result)
      return result

-- | Invalidate cache entries for a specific workflow
invalidateCache :: Monad m => Cache m -> Text -> m ()
invalidateCache cache workflow = do
  -- In a real implementation, we'd filter by workflow
  -- For now, this is a simple clear
  cacheClear cache