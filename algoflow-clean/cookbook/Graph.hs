{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : Graph
Description : Simple computational graphs

The simplest thing that could possibly work.
A graph is just a way to describe computation order.
-}

module Graph 
  ( -- * Core types
    Graph(..)
  , Node(..)
    
    -- * Building graphs
  , node
  , (~>)
  , merge
  , par
    
    -- * Running graphs  
  , run
  , runPar
    
    -- * FSM support
  , FSM(..)
  , fsm
  , runFSM
  ) where

import Control.Arrow
import Control.Category
import Control.Monad ((>=>))
import Control.Concurrent.Async (concurrently)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Prelude hiding (id, (.))

-- | A computation graph is just a description of how to compute something
data Graph m a b where
  -- | Lift a pure function
  Pure :: (a -> b) -> Graph m a b
  
  -- | Effectful computation  
  Effect :: (a -> m b) -> Graph m a b
  
  -- | Sequential composition (the key operation!)
  Seq :: Graph m a b -> Graph m b c -> Graph m a c
  
  -- | Parallel composition
  Par :: Graph m a b -> Graph m c d -> Graph m (a,c) (b,d)
  
  -- | Merge parallel results
  Merge :: (b -> c -> d) -> Graph m a b -> Graph m a c -> Graph m a d
  
  -- | Named node (for debugging/caching)
  Node :: String -> Graph m a b -> Graph m a b

-- | A node is just a named computation
node :: String -> (a -> m b) -> Graph m a b
node name f = Node name (Effect f)

-- | Sequential composition (like >>>)
(~>) :: Graph m a b -> Graph m b c -> Graph m a c
(~>) = Seq
infixr 1 ~>

-- | Merge two computations on the same input
merge :: (b -> c -> d) -> Graph m a b -> Graph m a c -> Graph m a d
merge = Merge

-- | Parallel composition on different inputs
par :: Graph m a b -> Graph m c d -> Graph m (a,c) (b,d)
par = Par

-- | Run a graph (sequential interpreter)
run :: Monad m => Graph m a b -> a -> m b
run (Pure f) a = return (f a)
run (Effect f) a = f a
run (Seq g1 g2) a = run g1 a >>= run g2
run (Par g1 g2) (a, c) = do
  b <- run g1 a
  d <- run g2 c
  return (b, d)
run (Merge f g1 g2) a = do
  b <- run g1 a
  c <- run g2 a
  return (f b c)
run (Node _ g) a = run g a

-- | Run a graph with parallelism where possible
runPar :: Graph IO a b -> a -> IO b
runPar (Pure f) a = return (f a)
runPar (Effect f) a = f a
runPar (Seq g1 g2) a = runPar g1 a >>= runPar g2
runPar (Par g1 g2) (a, c) = do
  -- Actually run in parallel!
  (b, d) <- concurrently (runPar g1 a) (runPar g2 c)
  return (b, d)
runPar (Merge f g1 g2) a = do
  -- Run both branches in parallel
  (b, c) <- concurrently (runPar g1 a) (runPar g2 a)
  return (f b c)
runPar (Node _ g) a = runPar g a

-- | Category instance - graphs compose!
instance Monad m => Category (Graph m) where
  id = Pure id
  (.) = flip Seq

-- | Arrow instance - for parallel composition
instance Monad m => Arrow (Graph m) where
  arr = Pure
  first g = Par g (Pure id)
  second g = Par (Pure id) g
  (***) = Par

-- | Finite State Machine support
-- An FSM is just a graph from (State, Input) to (State, Output)
data FSM m state input output = FSM
  { fsmInitial :: state
  , fsmTransition :: Graph m (state, input) (state, output)
  }

-- | Create an FSM
fsm :: state -> Graph m (state, input) (state, output) -> FSM m state input output
fsm = FSM

-- | Run an FSM on a list of inputs
runFSM :: Monad m => FSM m s i o -> [i] -> m (s, [o])
runFSM (FSM initial transition) inputs = go initial inputs []
  where
    go state [] outputs = return (state, reverse outputs)
    go state (i:is) outputs = do
      (state', output) <- run transition (state, i)
      go state' is (output:outputs)

-- Examples showing this is actually simple:

-- | Example 1: Simple data pipeline
simplePipeline :: Graph IO String Int
simplePipeline = 
  node "read" return ~>
  node "parse" (return . length) ~>
  node "double" (return . (*2))

-- | Example 2: Parallel processing
parallelPipeline :: Graph IO (String, String) (Int, Int)
parallelPipeline = 
  par (node "count1" (return . length))
      (node "count2" (return . length))

-- | Example 3: Merge pattern
mergePipeline :: Graph IO String Int
mergePipeline = merge (+)
  (node "words" (return . length . words))
  (node "lines" (return . length . lines))

-- | Example 4: Simple FSM (counter)
counterFSM :: FSM IO Int Bool Int
counterFSM = fsm 0 $ node "transition" $ \(count, increment) ->
  let newCount = if increment then count + 1 else count
  in return (newCount, count)

-- | Example 5: Graph of graphs (just composition!)
graphOfGraphs :: Graph IO String Int
graphOfGraphs = 
  simplePipeline ~>         -- This is a graph
  node "log" (\x -> print x >> return x) ~>
  Pure (*10)               -- More computation

-- The key insight: "graphs of graphs" is just function composition!
-- No magic needed, it's built into the language.
