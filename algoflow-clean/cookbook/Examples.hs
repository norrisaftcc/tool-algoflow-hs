{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{-|
Module      : Examples
Description : Real-world examples using the graph runner

Shows how common patterns map to our simple abstraction.
-}

module Examples where

import Control.Arrow
import Control.Category
import Control.Monad (forM_)
import Data.List (partition)
import Prelude hiding (id, (.))

import Graph

-- | Example 1: MapReduce pattern
mapReduce :: (a -> b) -> ([b] -> c) -> Graph IO [a] c
mapReduce mapper reducer = 
  -- Map phase: process each element
  node "map" (\xs -> return $ map mapper xs) ~>
  -- Reduce phase: combine results
  node "reduce" (\bs -> return $ reducer bs)

-- Usage:
wordCount :: Graph IO String Int
wordCount = 
  node "split" (return . words) ~>
  mapReduce length sum

-- | Example 2: Pipeline with error recovery
safeComputation :: Graph IO String Int
safeComputation = 
  node "parse" parseWithRecovery
  where
    parseWithRecovery s = 
      case reads s of
        [(n, "")] -> return n
        _ -> return 0  -- Default on parse error

-- | Example 3: Conditional execution (branching)
conditional :: (a -> Bool) -> Graph m a b -> Graph m a b -> Graph m a b
conditional predicate onTrue onFalse = node "cond" $ \a ->
  if predicate a
    then run onTrue a
    else run onFalse a

-- Example usage:
processNumber :: Graph IO Int String
processNumber = conditional (> 0)
  (node "positive" $ \n -> return $ "Positive: " ++ show n)
  (node "non-positive" $ \n -> return $ "Non-positive: " ++ show n)

-- | Example 4: Fan-out / Fan-in pattern
fanOutFanIn :: Graph m a b -> Graph m a c -> (b -> c -> d) -> Graph m a d
fanOutFanIn = merge

-- Real use case: parallel validation
validateData :: Graph IO UserData ValidationResult
validateData = fanOutFanIn
  (node "checkEmail" validateEmail)
  (node "checkAge" validateAge)
  combineResults
  where
    combineResults emailOk ageOk = 
      ValidationResult (emailOk && ageOk)

-- | Example 5: Recursive graphs (via FSM)
-- Retry logic as an FSM
retryFSM :: Int -> Graph IO a (Maybe b) -> FSM IO Int a (Maybe b)
retryFSM maxRetries computation = fsm 0 $ node "retry" $ \(attempts, input) -> do
  result <- run computation input
  case result of
    Just success -> return (attempts, Just success)
    Nothing -> 
      if attempts < maxRetries
        then return (attempts + 1, Nothing)
        else return (attempts, Nothing)

-- | Example 6: Streaming/Iterative processing
-- Process a stream of values, maintaining state
streamProcessor :: Graph IO (state, input) (state, output) -> state -> Graph IO [input] [output]
streamProcessor step initial = node "stream" $ \inputs -> do
  let go state [] = return []
      go state (i:is) = do
        (state', output) <- run step (state, i)
        outputs <- go state' is
        return (output : outputs)
  go initial inputs

-- Example: Running average
runningAverage :: Graph IO [Double] [Double]
runningAverage = streamProcessor avgStep (0, 0)
  where
    avgStep = node "avgStep" $ \((sum, count), value) ->
      let newSum = sum + value
          newCount = count + 1
          avg = newSum / fromIntegral newCount
      in return ((newSum, newCount), avg)

-- | Example 7: Graph transformations
-- Cache frequently used subgraphs
memoize :: Ord a => Graph IO a b -> Graph IO a b
memoize g = node "memoized" $ \a -> do
  -- In real implementation, check cache first
  run g a

-- | Example 8: Monitoring/Logging transformer
logged :: Show a => String -> Graph m a b -> Graph m a b
logged label g = Node (label ++ " [logged]") g

-- | Example 9: Rate limiting transformer
rateLimited :: Int -> Graph IO a b -> Graph IO a b
rateLimited delayMicros g = node "rateLimited" $ \a -> do
  result <- run g a
  threadDelay delayMicros
  return result

-- | Example 10: Complex workflow showing it all works together
complexWorkflow :: Graph IO FilePath Summary
complexWorkflow = 
  -- Read and parse file
  node "read" readFile ~>
  
  -- Process in parallel
  merge CombinedAnalysis
    (logged "wordAnalysis" $ 
      node "words" (return . words) ~>
      mapReduce id (length . filter ((> 5) . length)))
    (logged "lineAnalysis" $
      node "lines" (return . lines) ~>
      mapReduce (const 1) sum) ~>
  
  -- Final summary
  node "summarize" (\(CombinedAnalysis w l) -> 
    return $ Summary $ "Long words: " ++ show w ++ ", Lines: " ++ show l)

-- Types for examples
data UserData = UserData
data ValidationResult = ValidationResult Bool
validateEmail :: UserData -> IO Bool
validateEmail _ = return True
validateAge :: UserData -> IO Bool  
validateAge _ = return True

data CombinedAnalysis = CombinedAnalysis Int Int
newtype Summary = Summary String

threadDelay :: Int -> IO ()
threadDelay _ = return ()  -- Stub

-- | Running all examples
runExamples :: IO ()
runExamples = do
  putStrLn "=== Word Count ==="
  wc <- run wordCount "hello world from haskell"
  print wc
  
  putStrLn "\n=== Conditional ==="
  pos <- run processNumber 42
  neg <- run processNumber (-5)
  putStrLn pos
  putStrLn neg
  
  putStrLn "\n=== Running Average ==="
  avgs <- run runningAverage [1, 2, 3, 4, 5]
  print avgs
  
  putStrLn "\n=== Complex Workflow ==="
  -- Would run on actual file
  -- summary <- run complexWorkflow "data.txt"
  -- print summary
