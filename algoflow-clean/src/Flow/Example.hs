{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

{-|
Module      : Flow.Example
Description : Examples of using the workflow system

This shows how to use our clean, type-safe workflow system.
No ByteStrings everywhere, no stringly-typed nonsense.
-}

module Flow.Example where

import Control.Arrow
import Control.Category (>>>)
import Prelude hiding ((>>>))

import Flow.Core
import Flow.Free
import Flow.Execute

-- Example 1: Using the Arrow-based approach
-- This leverages Haskell's built-in Arrow notation

dataProcessingArrow :: Workflow IO FilePath ProcessedData
dataProcessingArrow = proc inputFile -> do
  -- Read the file
  rawData <- step "read" readFile -< inputFile
  
  -- Clean the data (in parallel if possible)
  cleaned <- step "clean" cleanData -< rawData
  validated <- step "validate" validateData -< rawData
  
  -- Merge results
  step "process" processData -< (cleaned, validated)

-- Example 2: Using the Free monad DSL
-- This is more flexible and easier to introspect

dataProcessingFree :: Flow.Free.Workflow FilePath ProcessedData
dataProcessingFree = do
  -- Sequential steps
  rawData <- compute "read" readFile
  
  -- Parallel processing
  (cleaned, validated) <- parallel
    (compute "clean" cleanData)
    (compute "validate" validateData)
  
  -- Cache expensive computation
  cache "processed" $ compute "process" $ \(c, v) -> 
    processData (c, v)

-- Example 3: Composing workflows
-- This shows the compositional nature

-- A simple workflow
parseConfig :: Workflow IO FilePath Config
parseConfig = step "parseConfig" $ \path -> do
  content <- readFile path
  return $ parseConfigContent content

-- Another simple workflow  
fetchData :: Workflow IO Config [DataItem]
fetchData = step "fetchData" $ \config -> do
  -- Fetch based on config
  fetchFromAPI (configEndpoint config)

-- Compose them!
fullPipeline :: Workflow IO FilePath ProcessedData
fullPipeline = 
  parseConfig >>> 
  fetchData >>> 
  step "analyze" analyzeItems

-- Example 4: Error handling done right
robustPipeline :: Flow.Free.Workflow FilePath ProcessedData
robustPipeline = recover mainPath fallbackPath
  where
    mainPath = do
      data' <- compute "fetch" fetchRemoteData
      compute "process" processData
    
    fallbackPath = do
      compute "loadCache" loadFromCache

-- Example 5: Using the graph-based approach for complex dependencies
-- (Would use Flow.Graph module)

-- Types for the examples
data ProcessedData = ProcessedData
data Config = Config { configEndpoint :: String }
data DataItem = DataItem

-- Stub implementations
cleanData :: String -> IO String
cleanData = return

validateData :: String -> IO Bool
validateData = return . const True

processData :: (String, Bool) -> IO ProcessedData
processData = return . const ProcessedData

parseConfigContent :: String -> Config
parseConfigContent _ = Config "http://example.com"

fetchFromAPI :: String -> IO [DataItem]
fetchFromAPI _ = return []

analyzeItems :: [DataItem] -> IO ProcessedData
analyzeItems _ = return ProcessedData

fetchRemoteData :: FilePath -> IO String
fetchRemoteData = readFile

loadFromCache :: FilePath -> IO ProcessedData
loadFromCache _ = return ProcessedData

-- Running the examples
runExamples :: IO ()
runExamples = do
  putStrLn "=== Arrow-based Workflow ==="
  result1 <- runWorkflow defaultConfig dataProcessingArrow "input.txt"
  print result1
  
  putStrLn "\n=== Free Monad Workflow ==="
  result2 <- Flow.Free.runWorkflow dataProcessingFree "input.txt"
  print result2
  
  putStrLn "\n=== Dry Run ==="
  putStrLn $ Flow.Free.dryRun robustPipeline
