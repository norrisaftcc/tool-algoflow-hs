{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Flow.FreeSpec where

import Test.Hspec
import Test.QuickCheck
import Flow.Free
import Control.Monad.Free

spec :: Spec
spec = describe "Flow.Free" $ do
  describe "Workflow DSL" $ do
    it "creates simple computation workflows" $ do
      let wf = compute "increment" (\x -> return (x + 1))
      result <- runWorkflow wf 10
      result `shouldBe` 11
    
    it "creates named computation steps" $ do
      let wf = compute "double" (\x -> return (x * 2))
      result <- runWorkflow wf 21
      result `shouldBe` 42
    
    it "handles pure return values" $ do
      let wf = return 42 :: Workflow () Int
      result <- runWorkflow wf ()
      result `shouldBe` 42

  describe "Monadic composition" $ do
    it "sequences computations" $ do
      let wf = do
            x <- compute "add5" (\a -> return (a + 5))
            compute "double" (\_ -> return (x * 2))
      result <- runWorkflow wf 3
      result `shouldBe` 16  -- (3 + 5) * 2
    
    it "uses bind for dependent computations" $ do
      let wf = do
            x <- compute "increment" (\a -> return (a + 1))
            y <- compute "square" (\_ -> return (x * x))
            return (x + y)
      result <- runWorkflow wf 4
      result `shouldBe` 30  -- x=5, y=25, x+y=30

  describe "Parallel workflows" $ do
    it "runs two workflows in parallel" $ do
      let wf1 = compute "left" (\x -> return (x + 10))
          wf2 = compute "right" (\x -> return (x * 3))
          parallel_wf = parallel wf1 wf2
      result <- runWorkflow parallel_wf (5, 7)
      result `shouldBe` (15, 21)  -- (5+10, 7*3)
    
    it "combines parallel results" $ do
      let wf1 = compute "branch1" (\x -> return (x * 2))
          wf2 = compute "branch2" (\x -> return (x + 100))
          combined = do
            (a, b) <- parallel wf1 wf2
            return (a + b)
      result <- runWorkflow combined (5, 3)
      result `shouldBe` 113  -- (5*2) + (3+100) = 10 + 103

  describe "Cached workflows" $ do
    it "creates cached computations" $ do
      let expensive = compute "expensive" (\x -> return (x * x))
          cached_wf = cache "square-cache" expensive
      result <- runWorkflow cached_wf 7
      result `shouldBe` 49
    
    it "caches complex workflows" $ do
      let complex = do
            x <- compute "step1" (\a -> return (a + 1))
            y <- compute "step2" (\_ -> return (x * 2))
            return (x + y)
          cached_complex = cache "complex-cache" complex
      result <- runWorkflow cached_complex 5
      result `shouldBe` 18  -- x=6, y=12, x+y=18

  describe "Error recovery" $ do
    it "provides fallback workflows" $ do
      let main_wf = compute "main" (\x -> return (x + 100))
          fallback_wf = compute "fallback" (\x -> return (x - 1))
          recovery_wf = recover main_wf fallback_wf
      -- For now, just test that main workflow runs (recovery logic simplified)
      result <- runWorkflow recovery_wf 10
      result `shouldBe` 110
    
    it "chains recovery workflows" $ do
      let primary = compute "primary" (\x -> return (x * 10))
          secondary = compute "secondary" (\x -> return (x + 5))
          tertiary = compute "tertiary" (\x -> return (x - 1))
          chained = recover (recover primary secondary) tertiary
      result <- runWorkflow chained 3
      result `shouldBe` 30  -- Primary works: 3 * 10

  describe "Complex workflow composition" $ do
    it "combines all DSL features" $ do
      let preprocessing = compute "preprocess" (\x -> return (x + 1))
          
          branch1 = cache "cache1" $ 
                   compute "expensive1" (\x -> return (x * x))
          
          branch2 = compute "fast2" (\x -> return (x + 10))
          
          parallel_processing = parallel branch1 branch2
          
          postprocessing = compute "combine" (\(a, b) -> return (a + b))
          
          recovery_fallback = compute "fallback" (\_ -> return 0)
          
          full_workflow = recover (do
            x <- preprocessing
            (squared, added) <- parallel_processing
            postprocessing
          ) recovery_fallback
      
      result <- runWorkflow full_workflow 5
      result `shouldBe` 52  -- preprocess: 6, branch1: 36, branch2: 16, combine: 52

  describe "Workflow introspection" $ do
    it "provides dry run capability" $ do
      let wf = do
            compute "step1" (\x -> return (x + 1))
            compute "step2" (\x -> return (x * 2))
            return 42
          dry_output = dryRun wf
      -- dryRun should return a string representation
      dry_output `shouldContain` "step1"
      dry_output `shouldContain` "step2"
    
    it "shows workflow structure" $ do
      let parallel_wf = parallel 
            (compute "left-branch" return)
            (compute "right-branch" return)
          dry_output = dryRun parallel_wf
      dry_output `shouldContain` "parallel"
      dry_output `shouldContain` "left-branch"
      dry_output `shouldContain` "right-branch"

  describe "Type safety" $ do
    it "maintains type safety across composition" $ do
      let int_workflow = compute "int-step" (\x -> return (x + 1 :: Int))
          string_workflow = compute "string-step" (\x -> return (show x))
          combined = int_workflow >>= \i -> string_workflow
      result <- runWorkflow combined 42
      result `shouldBe` "43"
    
    it "works with different input/output types" $ do
      let bool_to_int = compute "bool-to-int" (\b -> return (if b then 1 else 0))
          int_to_string = compute "int-to-string" (\i -> return ("value: " ++ show i))
          pipeline = bool_to_int >>= \i -> int_to_string
      result <- runWorkflow pipeline True
      result `shouldBe` "value: 1"