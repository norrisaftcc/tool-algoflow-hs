-- QuickStart.hs - Run this to see Flow.Simple in action!
-- 
-- To run:
--   cd algoflow-clean
--   cabal repl
--   :load ../examples/QuickStart.hs
--   main

import Flow.Simple
import System.Directory (doesFileExist)

-- Example 1: Simple sequential workflow
simpleDemo :: IO ()
simpleDemo = do
  putStrLn "=== Simple Sequential Workflow ==="
  
  let workflow = 
        step "get input" (return . (*2)) >>>
        step "transform" (return . show) >>>
        step "add prefix" (return . ("Result: " ++))
  
  result <- runWorkflow workflow 21
  putStrLn result  -- Output: "Result: 42"

-- Example 2: Parallel execution
parallelDemo :: IO ()
parallelDemo = do
  putStrLn "\n=== Parallel Execution ==="
  
  let workflow =
        step "count chars" (return . length) &&&
        step "get words" (return . words)
  
  (charCount, wordList) <- runWorkflow workflow "Hello world from Haskell"
  putStrLn $ "Characters: " ++ show charCount
  putStrLn $ "Words: " ++ show wordList

-- Example 3: Error handling
errorDemo :: IO ()
errorDemo = do
  putStrLn "\n=== Error Handling ==="
  
  let riskyWorkflow = 
        step "risky operation" (\x -> error "Oops!") `catch`
        step "fallback" (\x -> return $ "Recovered from error with input: " ++ x)
  
  result <- runWorkflow riskyWorkflow "test input"
  putStrLn result

-- Example 4: File processing with error handling
fileDemo :: IO ()
fileDemo = do
  putStrLn "\n=== File Processing ==="
  
  let fileWorkflow = 
        step "check exists" checkFile >>>
        step "read file" readFile >>>
        step "count words" (return . length . words) `catch`
        step "default" (const $ return 0)
      
      checkFile path = do
        exists <- doesFileExist path
        if exists 
          then return path
          else error $ "File not found: " ++ path
  
  -- Try with existing file (using this very file as an example)
  let demoFile = "QuickStart.hs"  -- Change this to any file you want to test
  count1 <- runWorkflow fileWorkflow demoFile
  putStrLn $ demoFile ++ " has " ++ show count1 ++ " words"
  
  -- Try with non-existent file (will use fallback)
  count2 <- runWorkflow fileWorkflow "nonexistent.txt"
  putStrLn $ "nonexistent.txt word count: " ++ show count2 ++ " (used fallback)"

-- Example 5: Complex workflow combining everything
complexDemo :: IO ()
complexDemo = do
  putStrLn "\n=== Complex Workflow ==="
  
  let analyzer = 
        step "prepare" prepare >>>
        (step "analyze length" analyzeLength &&&
         step "analyze content" analyzeContent) >>>
        step "combine results" combineResults `catch`
        step "error report" errorReport
      
      prepare s = return $ "Input: " ++ s
      
      analyzeLength s = return $ "Length = " ++ show (length s)
      
      analyzeContent s = return $ 
        "Has vowels = " ++ show (any (`elem` "aeiouAEIOU") s)
      
      combineResults (len, content) = return $
        "Analysis complete: " ++ len ++ ", " ++ content
      
      errorReport _ = return "Analysis failed"
  
  result <- runWorkflow analyzer "Hello AlgoFlow!"
  putStrLn result

-- Run all examples
main :: IO ()
main = do
  putStrLn "Welcome to Flow.Simple Quick Start!\n"
  
  simpleDemo
  parallelDemo  
  errorDemo
  fileDemo
  complexDemo
  
  putStrLn "\n✅ All examples completed!"
  putStrLn "\nTry modifying these examples to build your own workflows."