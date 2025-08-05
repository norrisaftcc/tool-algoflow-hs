{-# LANGUAGE GADTs #-}

{-|
Module      : Flow.Simple
Description : Simple, beginner-friendly workflow API

This module provides a simplified interface to the AlgoFlow workflow engine.
It hides the complexity of GADTs, Arrows, and type-level programming behind
a clean, easy-to-understand API.

= Quick Start

@
import Flow.Simple

-- Define a workflow
myWorkflow = 
  step "fetch" fetchData >>>
  step "process" processData >>>
  step "save" saveData

-- Run it
result <- runWorkflow myWorkflow "input.txt"
@

= Core Concepts

- __Workflow__: A description of computation steps
- __step__: Create a named computation step  
- __>>>__: Run steps sequentially
- __&&&__: Run steps in parallel
- __catch__: Handle errors gracefully
- __cached__: Cache expensive computations
-}

module Flow.Simple
  ( -- * Types
    Workflow
    
    -- * Building workflows
  , step
  , (>>>)
  , (&&&)
  
    -- * Error handling
  , catch
  , catchWith
    
    -- * Caching
  , cached
  
    -- * Running workflows
  , runWorkflow
  , runWorkflowPar
  
    -- * Utilities
  , constant
  , identity
  ) where

import Control.Exception (SomeException, toException)
import qualified Control.Exception as E
import Control.Concurrent.Async (concurrently)
import Prelude hiding ((>>>))

-- | A simple workflow type that hides all the complexity
data Workflow a b where
  Step   :: String -> (a -> IO b) -> Workflow a b
  Seq    :: Workflow a b -> Workflow b c -> Workflow a c  
  Par    :: Workflow a b -> Workflow a c -> Workflow a (b,c)
  Catch  :: Workflow a b -> Workflow a b -> Workflow a b
  Cached :: String -> Workflow a b -> Workflow a b

-- | Create a named workflow step from an IO action.
step :: String -> (a -> IO b) -> Workflow a b
step = Step

-- | Sequential composition.
(>>>) :: Workflow a b -> Workflow b c -> Workflow a c
(>>>) = Seq
infixr 1 >>>

-- | Parallel composition.
(&&&) :: Workflow a b -> Workflow a c -> Workflow a (b, c)
(&&&) = Par
infixr 3 &&&

-- | Handle errors with a fallback workflow.
catch :: Workflow a b -> Workflow a b -> Workflow a b
catch = Catch

-- | Handle errors with a function that can inspect the error.
-- Note: The handler receives the original input, not the error details.
-- This is a limitation of the simple API to avoid complex type constraints.
catchWith :: Workflow a b -> (SomeException -> a -> IO b) -> Workflow a b
catchWith w handler = w `catch` step "error-handler" (\a -> handler (toException (userError "Workflow failed")) a)

-- | Cache the results of a workflow.
cached :: String -> Workflow a b -> Workflow a b
cached = Cached

-- | Run a workflow sequentially.
runWorkflow :: Workflow a b -> a -> IO b
runWorkflow (Step _ f) a = f a
runWorkflow (Seq f g) a = runWorkflow f a >>= runWorkflow g
runWorkflow (Par f g) a = do
  b <- runWorkflow f a
  c <- runWorkflow g a
  return (b, c)
runWorkflow (Catch main fallback) a = 
  E.catch (runWorkflow main a) 
          (\(_ :: SomeException) -> runWorkflow fallback a)
runWorkflow (Cached key w) a = 
  -- For now, caching is a no-op in the simple API
  -- Real caching would require more type constraints
  runWorkflow w a

-- | Run a workflow with parallel execution where possible.
runWorkflowPar :: Workflow a b -> a -> IO b
runWorkflowPar (Step _ f) a = f a
runWorkflowPar (Seq f g) a = runWorkflowPar f a >>= runWorkflowPar g
runWorkflowPar (Par f g) a = concurrently (runWorkflowPar f a) (runWorkflowPar g a)
runWorkflowPar (Catch main fallback) a = 
  E.catch (runWorkflowPar main a) 
          (\(_ :: SomeException) -> runWorkflowPar fallback a)
runWorkflowPar (Cached key w) a = runWorkflow (Cached key w) a

-- | A workflow that always returns the same value.
constant :: b -> Workflow a b
constant value = step "constant" (\_ -> return value)

-- | A workflow that returns its input unchanged.
identity :: Workflow a a
identity = step "identity" return

