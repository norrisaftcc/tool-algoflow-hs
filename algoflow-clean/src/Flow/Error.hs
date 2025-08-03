{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Flow.Error
  ( -- * Error Types
    WorkflowError(..)
  , ErrorContext(..)
  , ErrorRecovery(..)
    
    -- * Error Handling Strategies
  , RetryPolicy(..)
  , defaultRetryPolicy
  , exponentialBackoff
    
    -- * Error Handling Functions
  , handleWorkflowError
  , withRetry
  , withCircuitBreaker
  ) where

import Control.Exception (Exception, SomeException, catch, throwIO)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime, diffUTCTime, NominalDiffTime)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Data.IORef

-- | Errors that can occur during workflow execution
data WorkflowError
  = StepFailed Text SomeException     -- ^ A workflow step failed with an exception
  | TimeoutError Text NominalDiffTime -- ^ Step exceeded time limit
  | ValidationError Text              -- ^ Input/output validation failed
  | CacheError Text                   -- ^ Cache operation failed
  | RecoveryFailed Text               -- ^ Recovery mechanism failed
  | CircuitBreakerOpen Text           -- ^ Circuit breaker is open
  deriving (Show, Typeable, Generic)

instance Exception WorkflowError

-- | Context for error reporting
data ErrorContext = ErrorContext
  { ecWorkflow :: Text          -- ^ Name of the workflow
  , ecStep :: Text              -- ^ Name of the step that failed
  , ecInput :: Text             -- ^ String representation of input
  , ecTimestamp :: UTCTime      -- ^ When the error occurred
  , ecAttempt :: Int            -- ^ Which attempt number
  } deriving (Show, Generic)

-- | How to recover from errors
data ErrorRecovery m a
  = NoRecovery                     -- ^ Don't attempt recovery
  | Retry (RetryPolicy m)          -- ^ Retry with policy
  | Fallback (m a)                 -- ^ Use fallback computation
  | RetryThenFallback (RetryPolicy m) (m a) -- ^ Try retry, then fallback

-- | Policy for retrying operations
data RetryPolicy m = RetryPolicy
  { rpMaxAttempts :: Int                    -- ^ Maximum number of attempts
  , rpDelay :: Int -> m ()                  -- ^ Delay function (attempt -> delay)
  , rpShouldRetry :: WorkflowError -> Bool -- ^ Should we retry this error?
  }

-- | Default retry policy: 3 attempts with fixed 1-second delay
defaultRetryPolicy :: MonadIO m => RetryPolicy m
defaultRetryPolicy = RetryPolicy
  { rpMaxAttempts = 3
  , rpDelay = \_ -> liftIO $ threadDelay 1000000  -- 1 second
  , rpShouldRetry = \err -> case err of
      StepFailed _ _ -> True
      TimeoutError _ _ -> True
      CircuitBreakerOpen _ -> False
      _ -> False
  }

-- | Exponential backoff retry policy
exponentialBackoff :: MonadIO m => Int -> RetryPolicy m
exponentialBackoff maxAttempts = RetryPolicy
  { rpMaxAttempts = maxAttempts
  , rpDelay = \attempt -> liftIO $ 
      threadDelay $ 1000000 * (2 ^ (attempt - 1))  -- 1s, 2s, 4s, ...
  , rpShouldRetry = \err -> case err of
      StepFailed _ _ -> True
      TimeoutError _ _ -> True
      CircuitBreakerOpen _ -> False
      _ -> False
  }

-- | Handle workflow errors with recovery strategy
handleWorkflowError :: MonadIO m 
                    => Text                  -- ^ Workflow name
                    -> Text                  -- ^ Step name
                    -> ErrorRecovery m a     -- ^ Recovery strategy
                    -> m a                   -- ^ Computation to run
                    -> m a
handleWorkflowError workflow step recovery action = 
  case recovery of
    NoRecovery -> action
    
    Retry policy -> withRetry workflow step policy action
    
    Fallback fallback -> 
      liftIO (catch (liftIO action) $ \(_ :: SomeException) -> liftIO fallback)
    
    RetryThenFallback policy fallback ->
      liftIO $ catch 
        (liftIO $ withRetry workflow step policy action)
        (\(_ :: WorkflowError) -> liftIO fallback)

-- | Execute an action with retry logic
withRetry :: MonadIO m
          => Text            -- ^ Workflow name
          -> Text            -- ^ Step name  
          -> RetryPolicy m   -- ^ Retry policy
          -> m a             -- ^ Action to retry
          -> m a
withRetry workflow step policy action = go 1
  where
    go attempt
      | attempt > rpMaxAttempts policy = 
          liftIO $ throwIO $ RecoveryFailed $ 
            T.concat [workflow, ".", step, " failed after ", T.pack (show $ rpMaxAttempts policy), " attempts"]
      | otherwise = do
          result <- liftIO $ catch (Right <$> liftIO action) $ \e -> 
            return $ Left $ StepFailed (workflow <> "." <> step) e
          
          case result of
            Right value -> return value
            Left err -> 
              if rpShouldRetry policy err && attempt < rpMaxAttempts policy
              then do
                rpDelay policy attempt
                go (attempt + 1)
              else liftIO $ throwIO err

-- | Circuit breaker state
data CircuitState = Closed | Open | HalfOpen
  deriving (Eq, Show)

data CircuitBreaker = CircuitBreaker
  { cbState :: MVar CircuitState
  , cbFailureCount :: IORef Int
  , cbLastFailureTime :: IORef (Maybe UTCTime)
  , cbThreshold :: Int
  , cbTimeout :: NominalDiffTime
  }

-- | Create a new circuit breaker
newCircuitBreaker :: Int -> NominalDiffTime -> IO CircuitBreaker
newCircuitBreaker threshold timeout = do
  state <- newMVar Closed
  failures <- newIORef 0
  lastFailure <- newIORef Nothing
  return $ CircuitBreaker state failures lastFailure threshold timeout

-- | Execute with circuit breaker protection
withCircuitBreaker :: MonadIO m
                   => Text            -- ^ Circuit name
                   -> CircuitBreaker  -- ^ Circuit breaker
                   -> m a             -- ^ Action to protect
                   -> m a
withCircuitBreaker name breaker action = do
  state <- liftIO $ readMVar (cbState breaker)
  
  case state of
    Open -> do
      -- Check if we should transition to half-open
      now <- liftIO getCurrentTime
      lastFailure <- liftIO $ readIORef (cbLastFailureTime breaker)
      case lastFailure of
        Just time | diffUTCTime now time > cbTimeout breaker -> do
          liftIO $ modifyMVar_ (cbState breaker) $ \_ -> return HalfOpen
          tryAction
        _ -> liftIO $ throwIO $ CircuitBreakerOpen name
    
    _ -> tryAction
  
  where
    tryAction = do
      result <- liftIO $ catch (Right <$> liftIO action) $ \e ->
        return $ Left $ StepFailed name e
      
      case result of
        Right value -> do
          -- Success: reset failure count and close circuit
          liftIO $ writeIORef (cbFailureCount breaker) 0
          liftIO $ modifyMVar_ (cbState breaker) $ \_ -> return Closed
          return value
          
        Left err -> do
          -- Failure: increment count and possibly open circuit
          failures <- liftIO $ atomicModifyIORef' (cbFailureCount breaker) $ \n -> (n + 1, n + 1)
          now <- liftIO getCurrentTime
          liftIO $ writeIORef (cbLastFailureTime breaker) (Just now)
          
          when (failures >= cbThreshold breaker) $
            liftIO $ modifyMVar_ (cbState breaker) $ \_ -> return Open
          
          liftIO $ throwIO err