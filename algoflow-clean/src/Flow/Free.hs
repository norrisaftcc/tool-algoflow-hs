{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : Flow.Free
Description : Free monad DSL for building workflows

Instead of the clunky string-based approach, we use a Free monad
to build a proper embedded DSL for workflows.
-}

module Flow.Free
  ( -- * Workflow DSL
    WorkflowF(..)
  , Workflow
  , WorkflowM
    
    -- * DSL Operations
  , compute
  , parallel
  , cache
  , recover
    
    -- * Interpreters
  , runWorkflow
  , runWorkflowWithCache
  , dryRun
  ) where

import Control.Monad.Free
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Flow.Cache as Cache
import Control.Exception (catch, SomeException)

-- | The functor that defines our workflow operations
data WorkflowF a b next where
  -- | Pure computation
  Compute :: Text -> (a -> IO b) -> (b -> next) -> WorkflowF a b next
  
  -- | Parallel execution of two workflows
  Parallel :: Workflow a b -> Workflow c d -> ((b, d) -> next) -> WorkflowF (a, c) (b, d) next
  
  -- | Cached computation
  Cached :: Text -> Workflow a b -> (b -> next) -> WorkflowF a b next
  
  -- | Error recovery
  Recover :: Workflow a b -> Workflow a b -> (b -> next) -> WorkflowF a b next

-- | Make it a functor
instance Functor (WorkflowF a b) where
  fmap f (Compute name comp next) = Compute name comp (f . next)
  fmap f (Parallel w1 w2 next) = Parallel w1 w2 (f . next)
  fmap f (Cached key w next) = Cached key w (f . next)
  fmap f (Recover w1 w2 next) = Recover w1 w2 (f . next)

-- | A workflow that transforms 'a' to 'b'
type Workflow a b = Free (WorkflowF a b) b

-- | Workflow monad for building computations
type WorkflowM a = forall b. Workflow a b

-- | Smart constructors for the DSL

-- | Create a named computation step
compute :: Text -> (a -> IO b) -> Workflow a b
compute name f = liftF $ Compute name f id

-- | Run two workflows in parallel
parallel :: Workflow a b -> Workflow c d -> Workflow (a, c) (b, d)
parallel w1 w2 = liftF $ Parallel w1 w2 id

-- | Cache the results of a workflow
cache :: Text -> Workflow a b -> Workflow a b
cache key w = liftF $ Cached key w id

-- | Add error recovery
recover :: Workflow a b -> Workflow a b -> Workflow a b
recover main fallback = liftF $ Recover main fallback id

-- | Interpreter that actually runs the workflow
runWorkflow :: Workflow a b -> a -> IO b
runWorkflow (Pure b) _ = return b
runWorkflow (Free step) input = case step of
  Compute name f next -> do
    result <- f input
    runWorkflow (next result) input
    
  Parallel w1 w2 next -> do
    -- In real implementation, these would run concurrently
    let (a, c) = input
    b <- runWorkflow w1 a
    d <- runWorkflow w2 c
    runWorkflow (next (b, d)) input
    
  Cached key w next -> do
    -- TODO: Implement actual caching
    result <- runWorkflow w input
    runWorkflow (next result) input
    
  Recover main fallback next -> do
    -- Try main workflow, use fallback on error
    result <- liftIO $ catch 
      (liftIO $ runWorkflow main input)
      (\(_ :: SomeException) -> liftIO $ runWorkflow fallback input)
    runWorkflow (next result) input

-- | Run a workflow with caching support
runWorkflowWithCache :: MonadIO m => Cache.Cache m -> Workflow a b -> a -> m b
runWorkflowWithCache cache = go
  where
    go :: MonadIO m => Workflow x y -> x -> m y
    go (Pure b) _ = return b
    go (Free f) input = case f of
      Compute name action next -> do
        result <- liftIO $ action input
        go (next result) input
        
      Parallel w1 w2 next -> do
        let (a, c) = input
        b <- go w1 a
        d <- go w2 c
        go (next (b, d)) input
        
      Cached key w next -> do
        -- Here we'd use the cache, but we need serialization
        -- For now, just run the workflow
        result <- go w input
        go (next result) input
        
      Recover main fallback next -> do
        -- Try main workflow, use fallback on error
        result <- liftIO $ catch 
          (liftIO $ go main input)
          (\(_ :: SomeException) -> liftIO $ go fallback input)
        go (next result) input

-- | Dry run interpreter that just describes what would happen
dryRun :: Workflow a b -> Text
dryRun = go 0
  where
    go :: Int -> Workflow a b -> Text
    go depth (Pure _) = indent depth <> "Pure result\n"
    go depth (Free step) = case step of
      Compute name _ next -> 
        indent depth <> "compute: " <> name <> "\n" <>
        go (depth + 1) (next undefined)
        
      Parallel w1 w2 next ->
        indent depth <> "parallel:\n" <>
        indent (depth + 1) <> "Branch 1:\n" <> go (depth + 2) w1 <>
        indent (depth + 1) <> "Branch 2:\n" <> go (depth + 2) w2 <>
        go depth (next undefined)
        
      Cached key w next ->
        indent depth <> "cache[" <> key <> "]:\n" <>
        go (depth + 1) w <>
        go depth (next undefined)
        
      Recover main fallback next ->
        indent depth <> "recover:\n" <>
        indent (depth + 1) <> "Try:\n" <> go (depth + 2) main <>
        indent (depth + 1) <> "Fallback:\n" <> go (depth + 2) fallback <>
        go depth (next undefined)
    
    indent n = T.replicate n "  "
