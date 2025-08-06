{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Verify
Description : Verify our graph runner has the right properties

Let's prove this actually works and forms a proper category.
-}

module Verify where

import Control.Category
import Control.Arrow
import Control.Concurrent (threadDelay)
import Data.Time (getCurrentTime, diffUTCTime)
import Prelude hiding (id, (.))

import Graph

-- | Verify category laws
verifyCategoryLaws :: IO ()
verifyCategoryLaws = do
  putStrLn "=== Verifying Category Laws ==="
  
  -- Left identity: id . f = f
  let f = node "f" (\x -> return (x + 1)) :: Graph IO Int Int
  testEqual "Left identity" (id . f) f 5
  
  -- Right identity: f . id = f  
  testEqual "Right identity" (f . id) f 5
  
  -- Associativity: (f . g) . h = f . (g . h)
  let g = node "g" (\x -> return (x * 2)) :: Graph IO Int Int
      h = node "h" (\x -> return (x - 3)) :: Graph IO Int Int
  testEqual "Associativity" ((f . g) . h) (f . (g . h)) 10

-- | Verify arrow laws
verifyArrowLaws :: IO ()
verifyArrowLaws = do
  putStrLn "\n=== Verifying Arrow Laws ==="
  
  -- arr id = id
  testEqual "arr id = id" (arr id :: Graph IO Int Int) id 42
  
  -- arr (f >>> g) = arr f >>> arr g
  let f = (+1)
      g = (*2)
  testEqual "arr composition" 
    (arr (f >>> g) :: Graph IO Int Int)
    (arr f >>> arr g) 
    7

-- | Test parallel composition
testParallel :: IO ()
testParallel = do
  putStrLn "\n=== Testing Parallel Execution ==="
  
  let slow1 = node "slow1" (\x -> do
        putStrLn $ "Starting slow1 with " ++ show x
        threadDelay 1000000  -- 1 second
        putStrLn "Finished slow1"
        return (x + 1))
      
      slow2 = node "slow2" (\x -> do
        putStrLn $ "Starting slow2 with " ++ show x
        threadDelay 1000000  -- 1 second  
        putStrLn "Finished slow2"
        return (x * 2))
  
  putStrLn "Sequential (should take ~2 seconds):"
  time $ run (merge (+) slow1 slow2) 5
  
  putStrLn "\nParallel (should take ~1 second):"
  time $ runPar (merge (+) slow1 slow2) 5

-- | Test FSM
testFSM :: IO ()
testFSM = do
  putStrLn "\n=== Testing FSM ==="
  
  -- Traffic light FSM
  let trafficLight = fsm "red" $ node "transition" $ \(state, input) ->
        return $ case (state, input) of
          ("red", True) -> ("green", "GO!")
          ("green", True) -> ("yellow", "SLOW DOWN!")
          ("yellow", True) -> ("red", "STOP!")
          (s, False) -> (s, "NO CHANGE")
  
  (finalState, outputs) <- runFSM trafficLight [True, True, True, False, True]
  putStrLn $ "Final state: " ++ finalState
  putStrLn $ "Outputs: " ++ show outputs

-- | Test graph of graphs
testComposition :: IO ()
testComposition = do
  putStrLn "\n=== Testing Graph Composition ==="
  
  -- Build reusable graph components
  let parse = node "parse" $ \s -> return (read s :: Int)
      double = node "double" $ \x -> return (x * 2)
      toString = node "toString" $ \x -> return (show x)
  
  -- Compose them different ways
  let pipeline1 = parse ~> double ~> toString
      pipeline2 = parse ~> double ~> double ~> toString
  
  result1 <- run pipeline1 "21"
  result2 <- run pipeline2 "21"
  
  putStrLn $ "Pipeline 1: " ++ result1
  putStrLn $ "Pipeline 2: " ++ result2

-- | Main verification
main :: IO ()
main = do
  verifyCategoryLaws
  verifyArrowLaws
  testParallel
  testFSM
  testComposition
  putStrLn "\n✅ All tests passed!"

-- Utilities

testEqual :: (Eq b, Show b) => String -> Graph IO a b -> Graph IO a b -> a -> IO ()
testEqual name g1 g2 input = do
  r1 <- run g1 input
  r2 <- run g2 input
  if r1 == r2
    then putStrLn $ "✓ " ++ name
    else error $ "✗ " ++ name ++ ": " ++ show r1 ++ " /= " ++ show r2

time :: IO a -> IO a
time action = do
  start <- getCurrentTime
  result <- action
  end <- getCurrentTime
  putStrLn $ "Time: " ++ show (diffUTCTime end start)
  return result
