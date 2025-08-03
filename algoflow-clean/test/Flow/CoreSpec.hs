{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Flow.CoreSpec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Flow.Core
import Control.Category
import Control.Arrow
import Prelude hiding ((.), id)

spec :: Spec
spec = describe "Flow.Core" $ do
  describe "Flow Arrow" $ do
    describe "Category laws" $ do
      it "satisfies left identity: id . f = f" $ property $ \(x :: Int) ->
        let f = Flow (\a -> return (a * 2))
        in runFlow (id . f) x `shouldReturnSame` runFlow f x
      
      it "satisfies right identity: f . id = f" $ property $ \(x :: Int) ->
        let f = Flow (\a -> return (a + 10))
        in runFlow (f . id) x `shouldReturnSame` runFlow f x
      
      it "satisfies associativity: (f . g) . h = f . (g . h)" $ property $ \(x :: Int) ->
        let f = Flow (\a -> return (a + 1))
            g = Flow (\a -> return (a * 2))
            h = Flow (\a -> return (a - 3))
        in runFlow ((f . g) . h) x `shouldReturnSame` runFlow (f . (g . h)) x

    describe "Arrow laws" $ do
      it "arr id = id" $ property $ \(x :: Int) ->
        runFlow (arr id) x `shouldReturnSame` runFlow id x
      
      it "arr (f >>> g) = arr f >>> arr g" $ property $ \(x :: Int) ->
        let addTen = (+10)
            multiplyBy2 = (*2)
        in runFlow (arr (addTen >>> multiplyBy2)) x `shouldReturnSame` runFlow (arr addTen >>> arr multiplyBy2) x
      
      it "first (arr f) = arr (first f)" $ property $ \(x :: Int, y :: String) ->
        let doubleInt = (*2)
        in runFlow (first (arr doubleInt)) (x, y) `shouldReturnSame` runFlow (arr (first doubleInt)) (x, y)

  describe "Workflow GADT" $ do
    describe "Basic workflow construction" $ do
      it "creates identity workflow" $ do
        let workflow = Id :: Workflow IO Int Int
            flow = interpret workflow
        runFlow flow 42 `shouldReturn` 42
      
      it "creates step workflow" $ do
        let workflow = step "increment" (\x -> return (x + 1))
            flow = interpret workflow
        runFlow flow 10 `shouldReturn` 11
      
      it "sequences workflows" $ do
        let w1 = step "add5" (\x -> return (x + 5))
            w2 = step "mul2" (\x -> return (x * 2))
            combined = Seq w1 w2
            flow = interpret combined
        runFlow flow 3 `shouldReturn` 16  -- (3 + 5) * 2
    
    describe "Parallel composition" $ do
      it "runs workflows in parallel" $ do
        let w1 = step "add10" (\x -> return (x + 10))
            w2 = step "mul3" (\x -> return (x * 3))
            parallel = Par w1 w2
            flow = interpret parallel
        runFlow flow (5, 7) `shouldReturn` (15, 21)  -- (5+10, 7*3)
    
    describe "Caching workflows" $ do
      it "creates cached workflow" $ do
        let baseWorkflow = step "expensive" (\x -> return (x * x))
            cachedWorkflow = cache "square" baseWorkflow
            flow = interpret cachedWorkflow
        runFlow flow 5 `shouldReturn` 25
    
    describe "Error recovery" $ do
      it "creates recovery workflow" $ do
        let mainWorkflow = step "main" (\x -> return (x + 100))
            fallbackWorkflow = step "fallback" (\x -> return (x - 1))
            recoveryWorkflow = recover mainWorkflow fallbackWorkflow
            flow = interpret recoveryWorkflow
        -- For now, just tests that main workflow runs (recovery not implemented)
        runFlow flow 10 `shouldReturn` 110

  describe "NamedFlow" $ do
    it "preserves flow name" $ do
      let namedFlow = NamedFlow "test-flow" (Flow (\x -> return (x + 1)))
      flowName namedFlow `shouldBe` "test-flow"
      runFlow (flowComputation namedFlow) 5 `shouldReturn` 6

  describe "Smart constructors" $ do
    it "step creates named workflow" $ do
      let workflow = step "double" (\x -> return (x * 2))
          flow = interpret workflow
      runFlow flow 21 `shouldReturn` 42
    
    it "cache creates cached workflow" $ do
      let baseWorkflow = step "compute" (\x -> return (show x))
          cachedWorkflow = cache "string-cache" baseWorkflow
          flow = interpret cachedWorkflow
      runFlow flow 123 `shouldReturn` "123"
    
    it "recover creates recovery workflow" $ do
      let main = step "primary" (\x -> return (x + 1))
          backup = step "secondary" (\x -> return (x - 1))
          recoveryWorkflow = recover main backup
          flow = interpret recoveryWorkflow
      runFlow flow 10 `shouldReturn` 11

  describe "Complex workflow composition" $ do
    it "combines sequential and parallel operations" $ do
      let preprocessing = step "preprocess" (\x -> return (x + 1))
          branch1 = step "branch1" (\x -> return (x * 2))
          branch2 = step "branch2" (\x -> return (x * 3))
          parallel = Par branch1 branch2
          postprocessing = step "postprocess" (\(a, b) -> return (a + b))
          
          -- Preprocess, then run parallel branches, then combine
          workflow = Seq (Seq (Seq preprocessing 
                                     (step "duplicate" (\x -> return (x, x))))
                                parallel)
                         postprocessing
          
          flow = interpret workflow
      -- Input 5: preprocess to 6, duplicate to (6,6), parallel to (12,18), combine to 30
      runFlow flow 5 `shouldReturn` 30

-- Helper for testing IO computations
shouldReturnSame :: (Eq a, Show a) => IO a -> IO a -> Property
shouldReturnSame actual expected = monadicIO $ do
  a <- run actual
  e <- run expected
  assert (a == e)