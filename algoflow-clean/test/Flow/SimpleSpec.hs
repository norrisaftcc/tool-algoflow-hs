{-# LANGUAGE OverloadedStrings #-}

module Flow.SimpleSpec (spec) where

import Test.Hspec
import Flow.Simple
import Control.Exception (throwIO, ErrorCall(..))

spec :: Spec
spec = do
  describe "Flow.Simple" $ do
    describe "Basic workflow construction" $ do
      it "creates and runs a simple workflow" $ do
        let workflow = step "double" (\x -> return (x * 2 :: Int))
        result <- runWorkflow workflow 21
        result `shouldBe` 42
      
      it "composes workflows sequentially" $ do
        let workflow = 
              step "double" (\x -> return (x * 2)) >>>
              step "add10" (\x -> return (x + 10)) >>>
              step "show" (\x -> return (show x))
        result <- runWorkflow workflow (5 :: Int)
        result `shouldBe` "20"
      
      it "composes workflows in parallel" $ do
        let workflow = 
              step "length" (\s -> return (length s)) &&&
              step "reverse" (\s -> return (reverse s))
        result <- runWorkflow workflow "hello"
        result `shouldBe` (5, "olleh")
    
    describe "Error handling" $ do
      it "catches errors with fallback" $ do
        let workflow = 
              step "fail" (\_ -> throwIO (ErrorCall "boom!")) `catch`
              step "recover" (\x -> return (x * 2))
        result <- runWorkflow workflow (21 :: Int)
        result `shouldBe` 42
      
      it "catches errors with handler function" $ do
        let workflow = 
              step "fail" (\_ -> throwIO (ErrorCall "boom!")) `catchWith` 
              \_ input -> return (input + 100)
        result <- runWorkflow workflow (5 :: Int)
        result `shouldBe` 105
    
    describe "Caching" $ do
      it "caches workflow results" $ do
        -- This is a simple test - real caching tested elsewhere
        let workflow = cached "test-cache" $
              step "compute" (\x -> return (x * 2))
        result1 <- runWorkflow workflow (21 :: Int)
        result2 <- runWorkflow workflow (21 :: Int)
        result1 `shouldBe` 42
        result2 `shouldBe` 42
    
    describe "Utility functions" $ do
      it "constant workflow always returns same value" $ do
        let workflow = constant "hello"
        result <- runWorkflow workflow ()
        result `shouldBe` "hello"
      
      it "identity workflow returns input unchanged" $ do
        let workflow = identity
        result <- runWorkflow workflow "test"
        result `shouldBe` "test"
    
    describe "Complex workflows" $ do
      it "combines all features" $ do
        let workflow = 
              step "parse" (\s -> return (read s :: Int)) >>>
              (step "double" (\x -> return (x * 2)) &&&
               step "triple" (\x -> return (x * 3))) >>>
              step "sum" (\(a, b) -> return (a + b)) `catch`
              constant 0
        
        result <- runWorkflow workflow "10"
        result `shouldBe` 50  -- (10*2) + (10*3) = 20 + 30 = 50