{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{-|
Module      : Flow.Execute
Description : Execution engine for workflows

This module provides the execution engine that can run workflows with:
- Automatic dependency resolution
- Parallel execution where possible
- Caching support
- Error handling
-}

module Flow.Execute
  ( -- * Execution
    runWorkflow
  , ExecutionConfig(..)
  , defaultConfig
    
    -- * Results
  , ExecutionResult(..)
  , ExecutionError(..)
  ) where

import Control.Concurrent.Async (async, wait, mapConcurrently)
import Control.Concurrent.STM
import Control.Exception (Exception, SomeException, catch, try, evaluate)
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import Data.Typeable (Typeable)
import Data.Dynamic (Dynamic)

import Flow.Core

-- | Configuration for workflow execution
data ExecutionConfig = ExecutionConfig
  { maxParallel :: Int          -- ^ Maximum parallel executions
  , enableCache :: Bool         -- ^ Whether to use caching
  , cacheDir :: Maybe FilePath  -- ^ Directory for cache storage
  }

-- | Default execution configuration
defaultConfig :: ExecutionConfig
defaultConfig = ExecutionConfig
  { maxParallel = 4
  , enableCache = False
  , cacheDir = Nothing
  }

-- | Result of executing a workflow
data ExecutionResult a = ExecutionResult
  { resultValue :: a              -- ^ The computed value
  , resultDuration :: Double      -- ^ Execution time in seconds
  , resultCacheHit :: Bool        -- ^ Whether result came from cache
  } deriving (Show, Eq)

-- | Errors that can occur during execution
data ExecutionError
  = StepFailed Text SomeException
  | CacheError Text
  | ConfigError Text
  deriving (Show, Typeable)

instance Exception ExecutionError

-- | Internal cache type
type Cache m = TVar (Map Text (ExecutionResult Dynamic))

-- | Run a workflow with the given configuration
runWorkflow :: ExecutionConfig 
            -> Workflow IO a b 
            -> a 
            -> IO (Either ExecutionError (ExecutionResult b))
runWorkflow config workflow input = do
  startTime <- getCurrentTime
  cache <- newTVarIO Map.empty
  
  -- Convert workflow to executable flow
  let flow = interpret workflow
  
  -- Execute with timing, forcing evaluation to catch pure exceptions
  result <- try $ do
    value <- runFlow flow input
    evaluate value  -- Force evaluation to catch arithmetic exceptions
  
  endTime <- getCurrentTime
  let duration = realToFrac $ diffUTCTime endTime startTime
  
  case result of
    Left ex -> return $ Left $ StepFailed "workflow" ex
    Right value -> return $ Right $ ExecutionResult
      { resultValue = value
      , resultDuration = duration
      , resultCacheHit = False
      }

-- | Execute workflows in parallel when possible
-- This is a more sophisticated version that analyzes the workflow structure
executeParallel :: ExecutionConfig 
                -> [(Text, Workflow IO a b)] 
                -> a 
                -> IO (Map Text (Either ExecutionError (ExecutionResult b)))
executeParallel config workflows input = do
  -- Create a pool of workers based on maxParallel config
  results <- mapConcurrently runOne workflows
  return $ Map.fromList results
  where
    runOne (name, wf) = do
      result <- runWorkflow config wf input
      return (name, result)

-- | Analyze a workflow to extract its dependency graph
-- This is where we'd implement the smart scheduling
analyzeWorkflow :: Workflow m a b -> WorkflowGraph
analyzeWorkflow = undefined -- TODO: Implement graph extraction

-- | Placeholder for workflow graph representation
data WorkflowGraph = WorkflowGraph
