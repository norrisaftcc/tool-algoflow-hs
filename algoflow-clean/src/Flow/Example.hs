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
import Control.Category
import Prelude hiding ((.), id)

import Flow.Core (Flow(..), runFlow, NamedFlow(..), step)
import qualified Flow.Core as Core
import qualified Flow.Free as Free  
import qualified Flow.Execute as Execute

-- Example 1: Using the Arrow-based approach
-- This leverages Haskell's built-in Arrow notation

dataProcessingArrow :: Core.Workflow IO FilePath ProcessedData
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

dataProcessingFree :: Free.Workflow FilePath ProcessedData
dataProcessingFree = do
  -- Sequential steps
  rawData <- Free.compute "read" readFile
  
  -- Parallel processing
  (cleaned, validated) <- Free.parallel
    (Free.compute "clean" cleanData)
    (Free.compute "validate" validateData)
  
  -- Cache expensive computation
  Free.cache "processed" $ Free.compute "process" $ \(c, v) -> 
    processData (c, v)

-- Example 3: Composing workflows
-- This shows the compositional nature

-- A simple workflow
parseConfig :: Core.Workflow IO FilePath Config
parseConfig = step "parseConfig" $ \path -> do
  content <- readFile path
  return $ parseConfigContent content

-- Another simple workflow  
fetchData :: Core.Workflow IO Config [DataItem]
fetchData = step "fetchData" $ \config -> do
  -- Fetch based on config
  fetchFromAPI (configEndpoint config)

-- Compose them!
fullPipeline :: Core.Workflow IO FilePath ProcessedData
fullPipeline = 
  parseConfig >>> 
  fetchData >>> 
  step "analyze" analyzeItems

-- Example 4: Error handling done right
robustPipeline :: Free.Workflow FilePath ProcessedData
robustPipeline = Free.recover mainPath fallbackPath
  where
    mainPath = do
      Free.compute "fetchAndProcess" (\path -> do
        data' <- fetchRemoteData path
        processData' data')
    
    fallbackPath = do
      Free.compute "loadCache" loadFromCache

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
  result1 <- Execute.runWorkflow Execute.defaultConfig dataProcessingArrow "input.txt"
  print result1
  
  putStrLn "\n=== Free Monad Workflow ==="
  result2 <- Free.runWorkflow dataProcessingFree "input.txt"
  print result2
  
  putStrLn "\n=== Dry Run ==="
  putStrLn $ Free.dryRun robustPipeline
