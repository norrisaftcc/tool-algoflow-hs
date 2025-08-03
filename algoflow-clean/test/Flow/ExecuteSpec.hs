{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Flow.ExecuteSpec where

import Test.Hspec
import Test.QuickCheck
import Flow.Execute
import Flow.Core
import Flow.Error
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Data.Either (isLeft, isRight)
import qualified Data.Map as Map

spec :: Spec
spec = describe "Flow.Execute" $ do
  describe "ExecutionEngine" $ do
    it "creates engine with initial state" $ do
      engine <- newEngine
      state <- readTVarIO (engineState engine)
      Map.null (runningTasks state) `shouldBe` True
      Map.null (taskResults state) `shouldBe` True
    
    it "tracks task execution" $ do
      engine <- newEngine
      let task = pure 42 :: Flow () Int
      taskId <- executeFlow engine "test-task" task ()
      
      -- Check task is running
      state <- readTVarIO (engineState engine)
      Map.member taskId (runningTasks state) `shouldBe` True
      
      -- Wait for completion
      result <- waitForTask engine taskId
      result `shouldBe` Right 42
      
      -- Check task is completed
      finalState <- readTVarIO (engineState engine)
      Map.member taskId (runningTasks finalState) `shouldBe` False
      Map.member taskId (taskResults finalState) `shouldBe` True

  describe "Task execution" $ do
    it "executes simple flows" $ do
      engine <- newEngine
      let flow = step "double" (*2) :: Flow Int Int
      taskId <- executeFlow engine "simple" flow 21
      result <- waitForTask engine taskId
      result `shouldBe` Right 42
    
    it "executes composed flows" $ do
      engine <- newEngine
      let flow = step "add1" (+1) >>> step "mul2" (*2) :: Flow Int Int
      taskId <- executeFlow engine "composed" flow 20
      result <- waitForTask engine taskId
      result `shouldBe` Right 42
    
    it "handles parallel execution" $ do
      engine <- newEngine
      let flow1 = step "slow1" (\x -> x + 1) :: Flow Int Int
          flow2 = step "slow2" (\x -> x * 2) :: Flow Int Int
          combined = flow1 &&& flow2
      taskId <- executeFlow engine "parallel" combined 10
      result <- waitForTask engine taskId
      result `shouldBe` Right (11, 20)

  describe "Error handling" $ do
    it "catches synchronous errors" $ do
      engine <- newEngine
      let flow = step "failing" (\(_ :: Int) -> do
                                    let x = 5 `div` 0  -- Controlled division by zero
                                    return x)
      taskId <- executeFlow engine "error-test" flow 42
      result <- waitForTask engine taskId
      result `shouldSatisfy` isLeft
      case result of
        Left (FlowExecutionError _ msg) -> msg `shouldContain` "divide by zero"
        _ -> expectationFailure "Expected FlowExecutionError"
    
    it "handles division by zero" $ do
      engine <- newEngine
      let flow = step "divide" (\x -> 100 `div` x) :: Flow Int Int
      taskId <- executeFlow engine "div-zero" flow 0
      result <- waitForTask engine taskId
      result `shouldSatisfy` isLeft
    
    it "propagates errors through composition" $ do
      engine <- newEngine
      let flow = step "safe" (+1) >>> 
                 step "unsafe" (\_ -> error "fail" :: Int) >>>
                 step "never-reached" (*2)
                 :: Flow Int Int
      taskId <- executeFlow engine "error-chain" flow 10
      result <- waitForTask engine taskId
      result `shouldSatisfy` isLeft

  describe "Task cancellation" $ do
    it "cancels running tasks" $ do
      engine <- newEngine
      let flow = step "slow" (\x -> x + 1) :: Flow Int Int
      taskId <- executeFlow engine "cancelable" flow 42
      
      -- Cancel immediately
      canceled <- cancelTask engine taskId
      canceled `shouldBe` True
      
      -- Verify task was removed
      state <- readTVarIO (engineState engine)
      Map.member taskId (runningTasks state) `shouldBe` False
    
    it "cannot cancel completed tasks" $ do
      engine <- newEngine
      let flow = pure 42 :: Flow () Int
      taskId <- executeFlow engine "quick" flow ()
      
      -- Wait for completion
      _ <- waitForTask engine taskId
      
      -- Try to cancel
      canceled <- cancelTask engine taskId
      canceled `shouldBe` False

  describe "Task status" $ do
    it "reports running status" $ do
      engine <- newEngine
      let flow = step "process" id :: Flow Int Int
      taskId <- executeFlow engine "status-test" flow 42
      
      -- Check immediate status
      status <- getTaskStatus engine taskId
      case status of
        Just Running -> pure ()
        Just (Completed _) -> pure ()  -- Might complete very fast
        _ -> expectationFailure "Expected Running or Completed status"
    
    it "reports completed status" $ do
      engine <- newEngine
      let flow = pure 123 :: Flow () Int
      taskId <- executeFlow engine "complete-test" flow ()
      
      -- Wait and check
      _ <- waitForTask engine taskId
      status <- getTaskStatus engine taskId
      status `shouldBe` Just (Completed (Right 123))
    
    it "reports failed status" $ do
      engine <- newEngine
      let flow = step "boom" (\(_ :: ()) -> error "failed" :: Int)
      taskId <- executeFlow engine "fail-test" flow ()
      
      -- Wait and check
      _ <- waitForTask engine taskId
      status <- getTaskStatus engine taskId
      case status of
        Just (Completed (Left _)) -> pure ()
        _ -> expectationFailure "Expected failed status"
    
    it "returns Nothing for unknown tasks" $ do
      engine <- newEngine
      let fakeId = TaskId "nonexistent"
      status <- getTaskStatus engine fakeId
      status `shouldBe` Nothing

  describe "Concurrent execution" $ do
    it "handles multiple concurrent tasks" $ do
      engine <- newEngine
      let flow = step "compute" (\x -> x * x) :: Flow Int Int
      
      -- Launch multiple tasks
      taskIds <- mapM (\i -> executeFlow engine ("task-" ++ show i) flow i) [1..10]
      
      -- Wait for all
      results <- mapM (waitForTask engine) taskIds
      
      -- Verify results
      let expected = [Right (i*i) | i <- [1..10]]
      results `shouldBe` expected
    
    it "maintains isolation between tasks" $ do
      engine <- newEngine
      
      -- Different flows with different types
      let flow1 = step "int-flow" (*2) :: Flow Int Int
          flow2 = step "string-flow" (++ "!") :: Flow String String
      
      task1 <- executeFlow engine "task1" flow1 21
      task2 <- executeFlow engine "task2" flow2 "hello"
      
      result1 <- waitForTask engine task1
      result2 <- waitForTask engine task2
      
      result1 `shouldBe` Right 42
      result2 `shouldBe` Right "hello!"

  describe "Resource cleanup" $ do
    it "cleans up completed tasks from state" $ do
      engine <- newEngine
      let flow = pure 42 :: Flow () Int
      
      -- Execute and wait
      taskId <- executeFlow engine "cleanup-test" flow ()
      _ <- waitForTask engine taskId
      
      -- Verify task is not in running tasks
      state <- readTVarIO (engineState engine)
      Map.member taskId (runningTasks state) `shouldBe` False
      
      -- But result is cached
      Map.member taskId (taskResults state) `shouldBe` True