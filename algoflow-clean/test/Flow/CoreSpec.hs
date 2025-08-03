{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Flow.CoreSpec where

import Test.Hspec
import Test.QuickCheck
import Flow.Core
import Control.Category
import Prelude hiding ((.), id)

spec :: Spec
spec = describe "Flow.Core" $ do
  describe "Flow Arrow" $ do
    describe "Category laws" $ do
      it "satisfies left identity: id . f = f" $ property $ \(x :: Int) ->
        let f = Flow (\a -> (a * 2, ["multiply by 2"]))
        in runFlow (id . f) x == runFlow f x
      
      it "satisfies right identity: f . id = f" $ property $ \(x :: Int) ->
        let f = Flow (\a -> (a + 10, ["add 10"]))
        in runFlow (f . id) x == runFlow f x
      
      it "satisfies associativity: (f . g) . h = f . (g . h)" $ property $ \(x :: Int) ->
        let f = Flow (\a -> (a + 1, ["add 1"]))
            g = Flow (\a -> (a * 2, ["multiply by 2"]))
            h = Flow (\a -> (a - 3, ["subtract 3"]))
        in runFlow ((f . g) . h) x == runFlow (f . (g . h)) x

    describe "Arrow laws" $ do
      it "arr id = id" $ property $ \(x :: Int) ->
        runFlow (arr id) x == runFlow id x
      
      it "arr (f >>> g) = arr f >>> arr g" $ property $ \(x :: Int) ->
        let f = (+10)
            g = (*2)
        in runFlow (arr (f >>> g)) x == runFlow (arr f >>> arr g) x
      
      it "first (arr f) = arr (first f)" $ property $ \(x :: Int, y :: String) ->
        let f = (*2)
        in runFlow (first (arr f)) (x, y) == runFlow (arr (first f)) (x, y)

    describe "Basic operations" $ do
      it "pure creates a flow with no dependencies" $ do
        let flow = pure @Flow 42
            (result, deps) = runFlow flow ()
        result `shouldBe` 42
        deps `shouldBe` []
      
      it "step creates a flow with a dependency" $ do
        let flow = step "increment" (+1)
            (result, deps) = runFlow flow 10
        result `shouldBe` 11
        deps `shouldBe` ["increment"]
      
      it "combines dependencies when composing flows" $ do
        let f1 = step "step1" (+1)
            f2 = step "step2" (*2)
            combined = f1 >>> f2
            (result, deps) = runFlow combined 5
        result `shouldBe` 12  -- (5 + 1) * 2
        deps `shouldBe` ["step1", "step2"]

    describe "Profunctor operations" $ do
      it "dimap correctly transforms input and output" $ do
        let flow = step "double" (*2)
            transformed = dimap (+1) (*10) flow
            (result, deps) = runFlow transformed 5
        result `shouldBe` 120  -- ((5 + 1) * 2) * 10
        deps `shouldBe` ["double"]
      
      it "lmap transforms input" $ do
        let flow = step "stringify" show
            transformed = lmap (*2) flow
            (result, deps) = runFlow transformed 5
        result `shouldBe` "10"
        deps `shouldBe` ["stringify"]
      
      it "rmap transforms output" $ do
        let flow = step "double" (*2)
            transformed = rmap (+100) flow
            (result, deps) = runFlow transformed 5
        result `shouldBe` 110  -- (5 * 2) + 100
        deps `shouldBe` ["double"]

  describe "WorkFlow class" $ do
    it "step creates named workflow steps" $ do
      let wf = step @Flow "test-step" (+42)
          (result, deps) = runFlow wf 8
      result `shouldBe` 50
      deps `shouldBe` ["test-step"]
    
    it "cache marks a flow as cacheable" $ do
      let wf = cache (step "expensive" (*2))
          (result, deps) = runFlow wf 21
      result `shouldBe` 42
      deps `shouldSatisfy` ("expensive" `elem`)