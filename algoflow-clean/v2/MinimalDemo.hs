module Main where

import Minimal
import Control.Exception (throwIO, ErrorCall(..))

-- Example 1: Simple pipeline
simplePipeline :: Flow String Int
simplePipeline = 
  step "get length" (return . length) >>>
  step "double it" (return . (*2)) >>>
  step "add 10" (return . (+10))

-- Example 2: Parallel processing
parallelAnalysis :: Flow String (Int, Int)
parallelAnalysis = 
  step "count words" (return . length . words) &&&
  step "count lines" (return . length . lines)

-- Example 3: Error handling
safeDiv :: Flow (Int, Int) Int
safeDiv = 
  step "divide" (\(a, b) -> 
    if b == 0 
    then throwIO (ErrorCall "Division by zero!")
    else return (a `div` b)) `catch`
  step "default" (const $ return 0)

-- Example 4: Cached computation
expensiveComputation :: Flow Int Int
expensiveComputation = cached "fibonacci" $
  step "compute" $ \n -> do
    putStrLn $ "Computing fibonacci(" ++ show n ++ ")..."
    return (fib n)
  where
    fib 0 = 0
    fib 1 = 1
    fib n = fib (n-1) + fib (n-2)

-- Example 5: Complex workflow showing all features
complexWorkflow :: Flow String Int
complexWorkflow = 
  cached "complex" $
    step "preprocess" (return . filter (/= ' ')) >>>
    (step "length" (return . length) &&&
     step "checksum" (return . sum . map fromEnum)) >>>
    step "combine" (\(len, sum) -> return (len + sum)) `catch`
    step "error fallback" (const $ return (-1))

main :: IO ()
main = do
  putStrLn "=== Minimal Workflow Engine Demo ==="
  
  -- Test 1: Simple pipeline
  putStrLn "\n1. Simple pipeline:"
  result1 <- runFlow simplePipeline "hello"
  putStrLn $ "  'hello' -> " ++ show result1
  
  -- Test 2: Parallel execution
  putStrLn "\n2. Parallel analysis:"
  result2 <- runFlowPar parallelAnalysis "hello\nworld\nhow are you"
  putStrLn $ "  (words, lines) = " ++ show result2
  
  -- Test 3: Error handling
  putStrLn "\n3. Error handling:"
  result3a <- runFlow safeDiv (10, 2)
  result3b <- runFlow safeDiv (10, 0)
  putStrLn $ "  10 / 2 = " ++ show result3a
  putStrLn $ "  10 / 0 = " ++ show result3b ++ " (caught error)"
  
  -- Test 4: Caching
  putStrLn "\n4. Caching demonstration:"
  result4a <- runFlowPar expensiveComputation 10
  putStrLn $ "  First call: " ++ show result4a
  result4b <- runFlowPar expensiveComputation 10
  putStrLn $ "  Second call: " ++ show result4b ++ " (from cache, no computation)"
  
  -- Test 5: Everything together
  putStrLn "\n5. Complex workflow:"
  result5 <- runFlowPar complexWorkflow "Hello World!"
  putStrLn $ "  Result: " ++ show result5
  
  putStrLn "\n✅ All 5 core requirements demonstrated!"