{-# LANGUAGE OverloadedStrings #-}

module Flow.ExecuteSpec where

import Test.Hspec
import Flow.Execute
import Flow.Core

spec :: Spec
spec = describe "Flow.Execute" $ do
  describe "ExecutionConfig" $ do
    it "creates default configuration" $ do
      let config = defaultConfig
      maxParallel config `shouldBe` 4
      enableCache config `shouldBe` False
      cacheDir config `shouldBe` Nothing
    
    it "allows custom configuration" $ do
      let config = ExecutionConfig
            { maxParallel = 8
            , enableCache = True
            , cacheDir = Just "/tmp/cache"
            }
      maxParallel config `shouldBe` 8
      enableCache config `shouldBe` True
      cacheDir config `shouldBe` Just "/tmp/cache"

  describe "Workflow execution" $ do
    it "executes simple workflows" $ do
      let workflow = step "add" (\x -> return (x + 1))
          config = defaultConfig
      result <- runWorkflow config workflow 5
      case result of
        Right (ExecutionResult value _ _ _) -> value `shouldBe` 6
        Left err -> expectationFailure $ "Expected success, got error: " ++ show err
    
    it "executes sequential workflows" $ do
      let w1 = step "add5" (\x -> return (x + 5))
          w2 = step "mul2" (\x -> return (x * 2))
          workflow = w1 >>> w2
          config = defaultConfig
      result <- runWorkflow config workflow 3
      case result of
        Right (ExecutionResult value _ _ _) -> value `shouldBe` 16  -- (3 + 5) * 2
        Left err -> expectationFailure $ "Expected success, got error: " ++ show err
    
    it "handles execution errors" $ do
      let workflow = step "divide" (\x -> return (10 `div` x))
          config = defaultConfig
      result <- runWorkflow config workflow 0
      case result of
        Left (ExecutionError _) -> return ()  -- Expected
        Right _ -> expectationFailure "Expected division by zero error"

  describe "ExecutionResult" $ do
    it "captures execution metadata" $ do
      let workflow = step "identity" return
          config = defaultConfig
      result <- runWorkflow config workflow "test"
      case result of
        Right execResult -> do
          resultValue execResult `shouldBe` "test"
          resultDuration execResult `shouldSatisfy` (>= 0)
        Left err -> expectationFailure $ "Expected success, got error: " ++ show err
    
    it "tracks execution time" $ do
      let workflow = step "delay" (\x -> do
                                    -- Small computation
                                    let _ = sum [1..1000]
                                    return x)
          config = defaultConfig
      result <- runWorkflow config workflow 42
      case result of
        Right execResult -> do
          resultValue execResult `shouldBe` 42
          resultDuration execResult `shouldSatisfy` (>= 0)
        Left err -> expectationFailure $ "Expected success, got error: " ++ show err

  describe "Error handling" $ do
    it "captures synchronous exceptions" $ do
      let workflow = step "fail" (\_ -> error "test error")
          config = defaultConfig
      result <- runWorkflow config workflow ()
      case result of
        Left (ExecutionError msg) -> msg `shouldContain` "test error"
        Right _ -> expectationFailure "Expected error to be captured"
    
    it "handles different error types" $ do
      let workflow = step "arithmetic-error" (\x -> return (1 `div` x))
          config = defaultConfig
      result <- runWorkflow config workflow 0
      result `shouldSatisfy` (\case
        Left (ExecutionError _) -> True
        Right _ -> False)

  describe "Parallel execution" $ do
    it "runs parallel workflows" $ do
      let w1 = step "left" (\x -> return (x + 10))
          w2 = step "right" (\x -> return (x * 2))
          workflow = w1 *** w2
          config = defaultConfig
      result <- runWorkflow config workflow (5, 7)
      case result of
        Right (ExecutionResult (left, right) _ _ _) -> do
          left `shouldBe` 15   -- 5 + 10
          right `shouldBe` 14  -- 7 * 2
        Left err -> expectationFailure $ "Expected success, got error: " ++ show err

  describe "Configuration effects" $ do
    it "respects parallel execution limits" $ do
      let workflow = step "simple" return
          config = ExecutionConfig 1 False Nothing
      result <- runWorkflow config workflow "test"
      case result of
        Right (ExecutionResult value _ _ _) -> value `shouldBe` "test"
        Left err -> expectationFailure $ "Expected success, got error: " ++ show err
    
    it "works with caching disabled" $ do
      let workflow = step "cacheable" (\x -> return (x * x))
          config = ExecutionConfig 4 False Nothing
      result <- runWorkflow config workflow 5
      case result of
        Right (ExecutionResult value _ _ _) -> value `shouldBe` 25
        Left err -> expectationFailure $ "Expected success, got error: " ++ show err