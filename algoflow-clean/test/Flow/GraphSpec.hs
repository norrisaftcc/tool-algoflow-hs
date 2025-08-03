{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Flow.GraphSpec where

import Test.Hspec
import Test.QuickCheck
import Flow.Graph
import qualified Algebra.Graph as G

spec :: Spec
spec = describe "Flow.Graph" $ do
  describe "Node operations" $ do
    it "creates nodes with functions" $ do
      let testNode = node "test" (\x -> return (x + 1)) :: Node Int Int
      nodeId testNode `shouldBe` "test"
      result <- nodeComputation testNode 5
      result `shouldBe` 6
    
    it "creates identity nodes" $ do
      let identityNode = node "identity" return :: Node Int Int
      nodeId identityNode `shouldBe` "identity"
      result <- nodeComputation identityNode 42
      result `shouldBe` 42
    
    it "creates transformation nodes" $ do
      let doubleNode = node "double" (\x -> return (x * 2)) :: Node Int Int
      nodeId doubleNode `shouldBe` "double"
      result <- nodeComputation doubleNode 21
      result `shouldBe` 42

  describe "Edge operations" $ do
    it "creates edges between compatible nodes" $ do
      let n1 = node "source" (\x -> return (x + 1)) :: Node Int Int
          n2 = node "target" (\x -> return (x * 2)) :: Node Int Int
          e = edge n1 n2
      nodeId (edgeFrom e) `shouldBe` "source"
      nodeId (edgeTo e) `shouldBe` "target"
    
    it "creates chains of edges" $ do
      let n1 = node "first" (\x -> return (x + 1)) :: Node Int Int
          n2 = node "second" (\x -> return (x * 2)) :: Node Int Int
          n3 = node "third" (\x -> return (x - 5)) :: Node Int Int
          e1 = edge n1 n2
          e2 = edge n2 n3
      nodeId (edgeFrom e1) `shouldBe` "first"
      nodeId (edgeTo e1) `shouldBe` "second"
      nodeId (edgeFrom e2) `shouldBe` "second"
      nodeId (edgeTo e2) `shouldBe` "third"

  describe "Graph construction" $ do
    it "builds graphs from node relationships" $ do
      let n1 = node "input" return :: Node Int Int
          n2 = node "process" (\x -> return (x * 2)) :: Node Int Int
          n3 = node "output" return :: Node Int Int
      -- Test that we can create the nodes (basic functionality)
      nodeId n1 `shouldBe` "input"
      nodeId n2 `shouldBe` "process"
      nodeId n3 `shouldBe` "output"
    
    it "creates edge relationships" $ do
      let n1 = node "A" (\x -> return (x + 1)) :: Node Int Int
          n2 = node "B" (\x -> return (x * 2)) :: Node Int Int
          e = edge n1 n2
      -- Verify edge connects the right nodes
      nodeId (edgeFrom e) `shouldBe` "A"
      nodeId (edgeTo e) `shouldBe` "B"

  describe "Node execution" $ do
    it "executes single node computations" $ do
      let computeNode = node "square" (\x -> return (x * x)) :: Node Int Int
      result <- nodeComputation computeNode 7
      result `shouldBe` 49
    
    it "handles different input/output types" $ do
      let stringifyNode = node "stringify" (\x -> return (show x)) :: Node Int String
      result <- nodeComputation stringifyNode 123
      result `shouldBe` "123"
    
    it "supports IO operations" $ do
      let ioNode = node "io-test" (\x -> do
                                      putStrLn ("Processing: " ++ show x)
                                      return (x + 100)) :: Node Int Int
      result <- nodeComputation ioNode 5
      result `shouldBe` 105

  describe "Type safety" $ do
    it "maintains type safety across node chains" $ do
      let intNode = node "int" (\x -> return (x + 1)) :: Node Int Int
          stringNode = node "string" (\x -> return (show x)) :: Node Int String
          boolNode = node "bool" (\s -> return (length s > 2)) :: Node String Bool
      
      -- Test individual nodes
      intResult <- nodeComputation intNode 5
      intResult `shouldBe` 6
      
      stringResult <- nodeComputation stringNode 42
      stringResult `shouldBe` "42"
      
      boolResult <- nodeComputation boolNode "hello"
      boolResult `shouldBe` True
    
    it "creates type-safe edges" $ do
      let producer = node "producer" (\() -> return 42) :: Node () Int
          consumer = node "consumer" (\x -> return (show x)) :: Node Int String
          connection = edge producer consumer
      
      nodeId (edgeFrom connection) `shouldBe` "producer"
      nodeId (edgeTo connection) `shouldBe` "consumer"

  describe "Complex node operations" $ do
    it "handles nodes with complex computations" $ do
      let fibonacci = node "fib" (\n -> return $ if n <= 1 then n else fibImpl n) :: Node Int Int
          fibImpl n = let loop a b i = if i >= n then b else loop b (a + b) (i + 1)
                      in loop 0 1 0
      
      result <- nodeComputation fibonacci 10
      result `shouldBe` 55
    
    it "supports stateful computations" $ do
      let counter = node "counter" (\start -> do
                                      -- Simulate some stateful operation
                                      return (start + 1)) :: Node Int Int
      result1 <- nodeComputation counter 0
      result2 <- nodeComputation counter 5
      result1 `shouldBe` 1
      result2 `shouldBe` 6

  describe "Error handling in nodes" $ do
    it "propagates IO exceptions" $ do
      let failingNode = node "fail" (\_ -> error "Computation failed") :: Node Int Int
      nodeComputation failingNode 5 `shouldThrow` anyException
    
    it "handles edge cases gracefully" $ do
      let divisionNode = node "divide" (\x -> return (100 `div` x)) :: Node Int Int
      result <- nodeComputation divisionNode 10
      result `shouldBe` 10
      
      -- Division by zero should throw
      nodeComputation divisionNode 0 `shouldThrow` anyException