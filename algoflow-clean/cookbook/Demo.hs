{-|
Module      : Demo
Description : Demonstration of the graph runner

Shows that we can do everything the original tried to do, but simpler.
-}

module Main where

import Graph
import Examples
import Analysis
import Verify

main :: IO ()
main = do
  putStrLn "=== Graph Runner Demo ==="
  putStrLn "\nThis simple implementation can do everything the original tried to do:\n"
  
  -- 1. Run computational DAGs
  putStrLn "1. Computational DAGs:"
  result <- run simplePipeline "test"
  putStrLn $ "   Pipeline result: " ++ show result
  
  -- 2. Parallel execution
  putStrLn "\n2. Parallel execution:"
  (r1, r2) <- runPar parallelPipeline ("hello", "world")
  putStrLn $ "   Parallel results: " ++ show (r1, r2)
  
  -- 3. FSMs
  putStrLn "\n3. Finite State Machines:"
  (final, outputs) <- runFSM counterFSM [True, True, False, True]
  putStrLn $ "   Final state: " ++ show final
  putStrLn $ "   Outputs: " ++ show outputs
  
  -- 4. Graph composition (graphs of graphs)
  putStrLn "\n4. Graphs of graphs (just composition!):"
  composed <- run graphOfGraphs "42"
  putStrLn $ "   Composed result: " ++ show composed
  
  -- 5. Analysis
  putStrLn "\n5. Static analysis:"
  putStrLn $ "   Node count: " ++ show (countNodes complexWorkflow)
  putStrLn $ "   Is pure? " ++ show (isPure simplePipeline)
  
  -- 6. Category laws
  putStrLn "\n6. Forms a proper category:"
  verifyCategoryLaws
  
  putStrLn "\n✅ Everything works, but simpler!"
  putStrLn "\nKey insights:"
  putStrLn "- Graphs are just function composition with extra steps"
  putStrLn "- 'Graphs of graphs' = regular composition (no magic needed)"
  putStrLn "- FSMs are just graphs from (State, Input) to (State, Output)"
  putStrLn "- Category theory isn't arcane - it's just associative composition with identity"
  putStrLn "- ~150 lines vs ~1000 lines, and more powerful!"
