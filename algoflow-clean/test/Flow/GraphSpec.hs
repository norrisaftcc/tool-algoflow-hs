{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Flow.GraphSpec where

import Test.Hspec
import Test.QuickCheck
import Flow.Graph
import qualified Algebra.Graph as G
import qualified Data.Map as Map
import Data.Maybe (fromJust)

spec :: Spec
spec = describe "Flow.Graph" $ do
  describe "Node operations" $ do
    it "creates nodes with functions" $ do
      let node = Node "test" (+1) :: Node Int Int
      nodeName node `shouldBe` "test"
      nodeFunc node 5 `shouldBe` 6
    
    it "creates input nodes" $ do
      let node = inputNode @Int "input"
      nodeName node `shouldBe` "input"
      nodeFunc node 42 `shouldBe` 42
    
    it "creates output nodes" $ do
      let node = outputNode @String "output"
      nodeName node `shouldBe` "output"
      nodeFunc node "test" `shouldBe` "test"

  describe "Edge operations" $ do
    it "creates edges between nodes" $ do
      let n1 = Node "source" (+1) :: Node Int Int
          n2 = Node "target" (*2) :: Node Int Int
          edge = Edge n1 n2
      edgeFrom edge `shouldBe` n1
      edgeTo edge `shouldBe` n2

  describe "WorkflowGraph construction" $ do
    it "builds graph from edges" $ do
      let n1 = inputNode @Int "input"
          n2 = Node "double" (*2)
          n3 = outputNode "output"
          edges = [Edge n1 n2, Edge n2 n3]
          wfGraph = workflowGraph edges
      G.vertexCount (graph wfGraph) `shouldBe` 3
      G.edgeCount (graph wfGraph) `shouldBe` 2
    
    it "includes all nodes in nodeMap" $ do
      let n1 = inputNode @Int "input"
          n2 = Node "process" (+10)
          edges = [Edge n1 n2]
          wfGraph = workflowGraph edges
          nodes = nodeMap wfGraph
      Map.size nodes `shouldBe` 2
      Map.member "input" nodes `shouldBe` True
      Map.member "process" nodes `shouldBe` True

  describe "Graph analysis" $ do
    it "performs topological sort" $ do
      let n1 = inputNode @Int "A"
          n2 = Node "B" (+1)
          n3 = Node "C" (*2)
          n4 = outputNode "D"
          edges = [Edge n1 n2, Edge n2 n3, Edge n3 n4, Edge n1 n3]
          wfGraph = workflowGraph edges
          sorted = topologicalSort wfGraph
      case sorted of
        Nothing -> expectationFailure "Expected successful topological sort"
        Just order -> do
          order `shouldSatisfy` ("A" `elem`)
          order `shouldSatisfy` ("D" `elem`)
          -- A must come before B and C
          let aIndex = fromJust $ lookup "A" (zip order [0..])
              bIndex = fromJust $ lookup "B" (zip order [0..])
              cIndex = fromJust $ lookup "C" (zip order [0..])
              dIndex = fromJust $ lookup "D" (zip order [0..])
          aIndex `shouldSatisfy` (< bIndex)
          aIndex `shouldSatisfy` (< cIndex)
          bIndex `shouldSatisfy` (< cIndex)
          cIndex `shouldSatisfy` (< dIndex)
    
    it "detects cycles" $ do
      let n1 = Node "A" (+1) :: Node Int Int
          n2 = Node "B" (*2)
          n3 = Node "C" (+3)
          edges = [Edge n1 n2, Edge n2 n3, Edge n3 n1]  -- cycle!
          wfGraph = workflowGraph edges
      hasCycle wfGraph `shouldBe` True
      topologicalSort wfGraph `shouldBe` Nothing
    
    it "identifies dependencies" $ do
      let n1 = inputNode @Int "input"
          n2 = Node "step1" (+1)
          n3 = Node "step2" (*2)
          n4 = Node "step3" (+10)
          n5 = outputNode "output"
          edges = [Edge n1 n2, Edge n2 n3, Edge n2 n4, Edge n3 n5, Edge n4 n5]
          wfGraph = workflowGraph edges
          deps = dependencies wfGraph "output"
      deps `shouldContain` ["input", "step1", "step2", "step3"]
      deps `shouldNotContain` ["output"]

  describe "Sequential workflow construction" $ do
    it "builds linear pipeline" $ do
      let wf = sequentialWorkflow
                [ inputNode @Int "start"
                , Node "add5" (+5)
                , Node "double" (*2)
                , Node "toString" show
                , outputNode "end"
                ]
          sorted = topologicalSort wf
      case sorted of
        Nothing -> expectationFailure "Sequential workflow should not have cycles"
        Just order -> order `shouldBe` ["start", "add5", "double", "toString", "end"]
    
    it "creates appropriate edges" $ do
      let nodes = [inputNode @Int "A", Node "B" id, outputNode "C"]
          wf = sequentialWorkflow nodes
      G.edgeCount (graph wf) `shouldBe` 2  -- A->B, B->C

  describe "Parallel workflow construction" $ do
    it "builds parallel branches" $ do
      let source = inputNode @Int "input"
          sink = outputNode @(Int, Int) "output"
          branches = 
            [ [Node "add10" (+10)]
            , [Node "mul2" (*2)]
            ]
          combiner = Node "combine" (\[a,b] -> (a,b))
          wf = parallelWorkflow source branches combiner sink
          sorted = topologicalSort wf
      case sorted of
        Nothing -> expectationFailure "Parallel workflow should not have cycles"
        Just order -> do
          -- Input comes first
          head order `shouldBe` "input"
          -- Output comes last
          last order `shouldBe` "output"
          -- Combiner comes after branches but before output
          let combineIdx = fromJust $ lookup "combine" (zip order [0..])
              add10Idx = fromJust $ lookup "add10" (zip order [0..])
              mul2Idx = fromJust $ lookup "mul2" (zip order [0..])
          add10Idx `shouldSatisfy` (< combineIdx)
          mul2Idx `shouldSatisfy` (< combineIdx)

  describe "Complex workflow examples" $ do
    it "handles diamond-shaped workflows" $ do
      let input = inputNode @Int "start"
          left = Node "left" (*2)
          right = Node "right" (+10)
          merge = Node "merge" (\[a,b] -> a + b)
          output = outputNode "end"
          edges = 
            [ Edge input left
            , Edge input right
            , Edge left merge
            , Edge right merge
            , Edge merge output
            ]
          wf = workflowGraph edges
      hasCycle wf `shouldBe` False
      dependencies wf "end" `shouldContain` ["start", "left", "right", "merge"]
      
    it "validates DAG properties" $ do
      property $ \(edges :: [(Int, Int)]) ->
        let nodes = [Node (show i) (+i) | i <- [0..9]] :: [Node Int Int]
            nodeList = take 10 $ cycle nodes
            validEdges = [Edge (nodeList !! from) (nodeList !! to) | 
                          (from, to) <- edges,
                          from < 10, to < 10, from /= to]
            wf = workflowGraph validEdges
        in hasCycle wf == (topologicalSort wf == Nothing)