{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Flow.Cache.Example where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import System.IO

import Flow.Core
import Flow.Cache

-- | Example of using the cache with a simple workflow
example :: IO ()
example = do
  -- Create an in-memory cache
  cache <- inMemoryCache
  
  -- Define a workflow that does expensive computation
  let expensiveWorkflow :: Workflow IO Int Int
      expensiveWorkflow = 
        step "double" (\x -> return (x * 2)) >>>
        cache "expensive-calc" (
          step "expensive" $ \x -> do
            putStrLn $ "Computing expensive operation for: " ++ show x
            -- Simulate expensive computation
            return (x * x)
        ) >>>
        step "format" (\x -> return x)
  
  -- Simple serialization for integers
  let serialize :: Int -> ByteString
      serialize = BS.pack . show
      
      deserialize :: ByteString -> Maybe Int
      deserialize bs = case reads (BS.unpack bs) of
        [(n, "")] -> Just n
        _ -> Nothing
  
  -- Run with cache
  putStrLn "=== First run (will compute) ==="
  result1 <- runFlow (interpretWithCache cache expensiveWorkflow) 5
  putStrLn $ "Result: " ++ show result1
  
  putStrLn "\n=== Second run (should use cache) ==="
  result2 <- runFlow (interpretWithCache cache expensiveWorkflow) 5
  putStrLn $ "Result: " ++ show result2
  
  -- Check cache statistics
  stats <- cacheStats cache
  putStrLn $ "\nCache statistics: " ++ show stats

-- | Example with multiple data types
multiTypeExample :: IO ()
multiTypeExample = do
  cache <- inMemoryCache
  
  -- Workflow that processes strings
  let stringWorkflow :: Workflow IO String Int
      stringWorkflow = 
        cache "string-length" (
          step "length" $ \s -> do
            putStrLn $ "Computing length of: " ++ s
            return (length s)
        )
  
  -- Workflow that processes numbers  
  let numberWorkflow :: Workflow IO Int String
      numberWorkflow =
        cache "number-format" (
          step "format" $ \n -> do
            putStrLn $ "Formatting number: " ++ show n
            return $ "Number: " ++ show n
        )
  
  -- Run both workflows
  len1 <- runFlow (interpret stringWorkflow) "hello"
  len2 <- runFlow (interpret stringWorkflow) "hello"  -- Would be cached if we had serialization
  
  str1 <- runFlow (interpret numberWorkflow) 42
  str2 <- runFlow (interpret numberWorkflow) 42  -- Would be cached if we had serialization
  
  putStrLn $ "String lengths: " ++ show [len1, len2]
  putStrLn $ "Formatted numbers: " ++ show [str1, str2]

-- | Example showing cache invalidation
invalidationExample :: IO ()
invalidationExample = do
  cache <- inMemoryCache
  
  let workflow = cache "computation" $
        step "compute" $ \x -> do
          putStrLn $ "Computing for: " ++ show x
          return (x * 10 :: Int)
  
  -- First run
  result1 <- runFlow (interpret workflow) 5
  putStrLn $ "First result: " ++ show result1
  
  -- Invalidate cache
  invalidateCache cache "computation"
  
  -- Second run (will recompute)
  result2 <- runFlow (interpret workflow) 5
  putStrLn $ "Second result (after invalidation): " ++ show result2