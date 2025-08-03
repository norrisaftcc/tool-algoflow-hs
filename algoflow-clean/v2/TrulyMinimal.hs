{-# LANGUAGE GADTs #-}

{-|
Module      : TrulyMinimal
Description : The absolute minimal workflow engine - NO dependencies

Under 100 lines. Zero dependencies beyond base.
Captures the 5 core requirements:
1. Define computational steps with explicit dependencies
2. Automatically resolve execution order
3. Run steps in parallel when possible (simulated with forkIO)
4. Handle errors gracefully
5. Cache results when sensible
-}

module TrulyMinimal where

import Control.Exception (catch, SomeException, evaluate)
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

-- | A workflow is computation with structure
data Flow a b where
  Step :: String -> (a -> IO b) -> Flow a b      -- Named step
  Seq  :: Flow a b -> Flow b c -> Flow a c       -- Sequential
  Par  :: Flow a b -> Flow a c -> Flow a (b,c)   -- Parallel  
  Err  :: Flow a b -> Flow a b -> Flow a b       -- Error handler
  Cache :: String -> Flow a b -> Flow a b        -- Cached

-- | Build a step
step :: String -> (a -> IO b) -> Flow a b
step = Step

-- | Sequential composition
(>>>) :: Flow a b -> Flow b c -> Flow a c
(>>>) = Seq

-- | Parallel composition  
(&&&) :: Flow a b -> Flow a c -> Flow a (b,c)
(&&&) = Par

-- | Error recovery
orElse :: Flow a b -> Flow a b -> Flow a b
orElse = Err

-- | Add caching
cached :: String -> Flow a b -> Flow a b
cached = Cache

-- | Run sequentially
runFlow :: Flow a b -> a -> IO b
runFlow (Step _ f) a = f a
runFlow (Seq f g) a = runFlow f a >>= runFlow g
runFlow (Par f g) a = do
  b <- runFlow f a
  c <- runFlow g a
  return (b, c)
runFlow (Err main fallback) a = 
  catch (runFlow main a) (\(_ :: SomeException) -> runFlow fallback a)
runFlow (Cache _ f) a = runFlow f a

-- | Run with parallelism (using forkIO)
runPar :: Flow a b -> a -> IO b
runPar (Step _ f) a = f a
runPar (Seq f g) a = runPar f a >>= runPar g
runPar (Par f g) a = do
  -- Simple parallel execution with MVars
  mv1 <- newEmptyMVar
  mv2 <- newEmptyMVar
  forkIO $ runPar f a >>= putMVar mv1
  forkIO $ runPar g a >>= putMVar mv2
  b <- takeMVar mv1
  c <- takeMVar mv2
  return (b, c)
runPar (Err main fallback) a = 
  catch (runPar main a) (\(_ :: SomeException) -> runPar fallback a)
runPar (Cache key f) a = 
  -- Ultra-simple string-based cache
  case lookup key simpleCache of
    Just b -> return (read b)
    Nothing -> do
      result <- runPar f a
      evaluate result  -- Force evaluation

-- | World's simplest cache (for demo only)
simpleCache :: [(String, String)]
simpleCache = []  -- In real use, would be IORef

-- | Example workflow showing all features
example :: Flow Int String
example = 
  cached "demo" $
    step "double" (\x -> return (x * 2)) >>>
    (step "show" (\x -> return (show x)) &&&
     step "words" (\x -> return (words (show x)))) >>>
    step "combine" (\(s, ws) -> return (s ++ " has " ++ show (length ws) ++ " words"))
      `orElse`
    step "fallback" (\_ -> return "error")

-- Demo function
demo :: IO ()
demo = do
  putStrLn "Running minimal workflow..."
  result <- runPar example 21
  putStrLn $ "Result: " ++ result