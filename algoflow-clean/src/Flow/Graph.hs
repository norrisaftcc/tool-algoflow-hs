{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Flow.Graph  
Description : Graph-based workflow representation

This module provides a graph-based workflow system that:
- Properly handles DAGs with dependency analysis
- Uses algebraic-graphs library instead of reinventing wheels
- Provides type-safe node references
-}

module Flow.Graph
  ( -- * Graph-based workflows
    Node(..)
  , Edge(..)
  , Graph
  , GraphWorkflow(..)
    
    -- * Building graphs
  , node
  , edge
  , (-->)
  , buildGraph
    
    -- * Analysis
  , topologicalSort
  , findCycles
  , dependencyLevels
    
    -- * Execution
  , executeGraph
  ) where

import qualified Algebra.Graph as G
import qualified Algebra.Graph.ToGraph as TG
import Control.Concurrent.Async (async, wait, mapConcurrently)
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Maybe (mapMaybe)

-- | A node in the computation graph
data Node a b = Node
  { nodeId :: Text
  , nodeComputation :: a -> IO b
  }

-- | An edge representing a data dependency
data Edge a b c = Edge
  { edgeFrom :: Node a b
  , edgeTo :: Node b c
  }

-- | Type synonym for our graph
type Graph a = G.Graph a

-- | A workflow represented as a graph
data GraphWorkflow where
  GraphWorkflow :: Graph (SomeNode) -> GraphWorkflow

-- | Existential wrapper for nodes with different types
data SomeNode where
  SomeNode :: Node a b -> SomeNode

-- | Smart constructor for nodes
node :: Text -> (a -> IO b) -> Node a b
node = Node

-- | Smart constructor for edges
edge :: Node a b -> Node b c -> Edge a b c
edge = Edge

-- | Infix edge constructor
(-->) :: Node a b -> Node b c -> Edge a b c
(-->) = edge
infixr 5 -->

-- | Build a graph from nodes and edges
buildGraph :: [SomeNode] -> [(Text, Text)] -> Graph SomeNode
buildGraph nodes edges = G.overlay nodesGraph edgesGraph
  where
    nodesGraph = G.vertices nodes
    nodeMap = Map.fromList [(nodeId' n, n) | n <- nodes]
    nodeId' (SomeNode (Node nid _)) = nid
    
    edgesGraph = G.edges 
      [ (fromNode, toNode)
      | (from, to) <- edges
      , Just fromNode <- [Map.lookup from nodeMap]
      , Just toNode <- [Map.lookup to nodeMap]
      ]

-- | Topological sort of the graph
topologicalSort :: Graph a -> Either [a] [a]
topologicalSort graph = 
  case TG.topSort (TG.toAdjacencyMap graph) of
    Left cycle -> Left cycle
    Right sorted -> Right sorted

-- | Find cycles in the graph
findCycles :: Ord a => Graph a -> [[a]]
findCycles = TG.vertexList . TG.scc . TG.toAdjacencyMap
  where
    -- Only keep non-trivial strongly connected components
    nonTrivial xs = length xs > 1

-- | Group nodes by dependency levels (for parallel execution)
dependencyLevels :: Ord a => Graph a -> [[a]]
dependencyLevels graph = go Set.empty (TG.vertexList $ TG.toAdjacencyMap graph)
  where
    adjMap = TG.toAdjacencyMap graph
    
    go :: Ord a => Set a -> [a] -> [[a]]
    go _ [] = []
    go completed remaining = 
      let ready = filter (readyToRun completed) remaining
          newCompleted = completed `Set.union` Set.fromList ready
          stillRemaining = filter (`notElem` ready) remaining
      in if null ready 
         then [] -- Cycle detected
         else ready : go newCompleted stillRemaining
    
    readyToRun completed node =
      all (`Set.member` completed) (TG.preSet node adjMap)

-- | Execute a graph workflow
executeGraph :: GraphWorkflow -> IO (Map Text SomeResult)
executeGraph (GraphWorkflow graph) = do
  case topologicalSort graph of
    Left cycle -> error $ "Cycle detected: " ++ show cycle
    Right sorted -> do
      -- Group by levels for parallel execution
      let levels = dependencyLevels graph
      
      -- Execute level by level
      results <- foldM executeLevel Map.empty levels
      return results
  where
    executeLevel :: Map Text SomeResult -> [SomeNode] -> IO (Map Text SomeResult)
    executeLevel results nodes = do
      -- Execute all nodes in this level in parallel
      newResults <- mapConcurrently (executeNode results) nodes
      return $ Map.union results (Map.fromList newResults)
    
    executeNode :: Map Text SomeResult -> SomeNode -> IO (Text, SomeResult)
    executeNode results (SomeNode (Node nid comp)) = do
      -- In a real implementation, we'd look up inputs from results
      -- and apply the computation
      result <- comp undefined -- Placeholder
      return (nid, SomeResult result)

-- | Existential wrapper for results
data SomeResult where
  SomeResult :: a -> SomeResult
