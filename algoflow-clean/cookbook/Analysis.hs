{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : Analysis
Description : Graph analysis and optimization

Since our graphs are just data, we can analyze and transform them!
-}

module Analysis where

import Graph

-- | Count nodes in a graph
countNodes :: Graph m a b -> Int
countNodes (Pure _) = 0
countNodes (Effect _) = 1  
countNodes (Seq g1 g2) = countNodes g1 + countNodes g2
countNodes (Par g1 g2) = countNodes g1 + countNodes g2
countNodes (Merge _ g1 g2) = countNodes g1 + countNodes g2
countNodes (Node _ g) = countNodes g

-- | Extract node names for visualization
extractNodes :: Graph m a b -> [String]
extractNodes (Pure _) = []
extractNodes (Effect _) = ["<effect>"]
extractNodes (Seq g1 g2) = extractNodes g1 ++ extractNodes g2
extractNodes (Par g1 g2) = extractNodes g1 ++ extractNodes g2
extractNodes (Merge _ g1 g2) = extractNodes g1 ++ extractNodes g2
extractNodes (Node name g) = [name]

-- | Optimize a graph by fusing sequential pure functions
optimize :: Graph m a b -> Graph m a b
optimize (Seq (Pure f) (Pure g)) = Pure (g . f)  -- Fuse pure functions
optimize (Seq g1 g2) = Seq (optimize g1) (optimize g2)
optimize (Par g1 g2) = Par (optimize g1) (optimize g2)
optimize (Merge f g1 g2) = Merge f (optimize g1) (optimize g2)
optimize (Node name g) = Node name (optimize g)
optimize g = g

-- | Check if a graph is pure (no effects)
isPure :: Graph m a b -> Bool
isPure (Pure _) = True
isPure (Effect _) = False
isPure (Seq g1 g2) = isPure g1 && isPure g2
isPure (Par g1 g2) = isPure g1 && isPure g2
isPure (Merge _ g1 g2) = isPure g1 && isPure g2
isPure (Node _ g) = isPure g

-- | Convert graph to DOT format for visualization
toDot :: Graph m a b -> String
toDot g = "digraph G {\n" ++ go 0 g ++ "}"
  where
    go :: Int -> Graph m a b -> String
    go n (Pure _) = "  n" ++ show n ++ " [label=\"pure\"];\n"
    go n (Effect _) = "  n" ++ show n ++ " [label=\"effect\"];\n"
    go n (Seq g1 g2) = 
      let n1 = n + 1
          n2 = n1 + size g1
      in go n1 g1 ++ go n2 g2 ++ 
         "  n" ++ show n1 ++ " -> n" ++ show n2 ++ ";\n"
    go n (Par g1 g2) =
      let n1 = n + 1
          n2 = n1 + size g1
      in go n1 g1 ++ go n2 g2 ++ 
         "  n" ++ show n ++ " [label=\"par\"];\n" ++
         "  n" ++ show n ++ " -> n" ++ show n1 ++ ";\n" ++
         "  n" ++ show n ++ " -> n" ++ show n2 ++ ";\n"
    go n (Node name g) = 
      "  n" ++ show n ++ " [label=\"" ++ name ++ "\"];\n" ++
      go (n + 1) g
    go n (Merge _ g1 g2) =
      let n1 = n + 1
          n2 = n1 + size g1
      in go n1 g1 ++ go n2 g2 ++
         "  n" ++ show n ++ " [label=\"merge\"];\n" ++
         "  n" ++ show n1 ++ " -> n" ++ show n ++ ";\n" ++
         "  n" ++ show n2 ++ " -> n" ++ show n ++ ";\n"
    
    size :: Graph m a b -> Int
    size (Pure _) = 1
    size (Effect _) = 1
    size (Seq g1 g2) = 1 + size g1 + size g2
    size (Par g1 g2) = 1 + size g1 + size g2
    size (Merge _ g1 g2) = 1 + size g1 + size g2
    size (Node _ g) = 1 + size g

-- | Static analysis: find potential parallelization opportunities
findParallelizable :: Graph m a b -> [(String, String)]
findParallelizable g = []  -- Simplified for now

-- | Transform a graph to maximize parallelism
parallelize :: Graph m a b -> Graph m a b
parallelize = id  -- Simplified for now

-- The key insight: Since graphs are just data (ADTs), we can:
-- 1. Analyze them statically
-- 2. Transform/optimize them
-- 3. Generate visualizations
-- 4. Prove properties about them

-- This is the power of the functional approach!
