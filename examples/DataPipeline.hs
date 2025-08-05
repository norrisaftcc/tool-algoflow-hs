-- DataPipeline.hs - A practical example of data processing with Flow.Simple
--
-- This example shows how to build a CSV processing pipeline that:
-- 1. Validates input files
-- 2. Parses CSV data
-- 3. Transforms records
-- 4. Generates summary statistics
-- 5. Writes results with error handling

import Flow.Simple
import Data.List (intercalate)
import qualified Data.Char as Char
import System.Directory (doesFileExist)

-- Our data types
data CSVConfig = CSVConfig
  { inputPath :: FilePath
  , outputPath :: FilePath
  , delimiter :: Char
  } deriving (Show)

data CSVData = CSVData
  { headers :: [String]
  , rows :: [[String]]
  } deriving (Show)

data Summary = Summary
  { totalRows :: Int
  , totalColumns :: Int
  , emptyCells :: Int
  } deriving (Show)

-- Main pipeline
csvPipeline :: Workflow CSVConfig Summary
csvPipeline = 
  step "validate config" validateConfig >>>
  step "read CSV file" readCSV >>>
  step "clean data" cleanData >>>
  step "analyze data" analyzeData >>>
  step "write summary" writeSummary

-- Step implementations
validateConfig :: CSVConfig -> IO CSVConfig
validateConfig config = do
  exists <- doesFileExist (inputPath config)
  if exists
    then do
      putStrLn $ "✓ Found input file: " ++ inputPath config
      return config
    else error $ "Input file not found: " ++ inputPath config

readCSV :: CSVConfig -> IO (CSVConfig, CSVData)
readCSV config = do
  content <- readFile (inputPath config)
  let allLines = lines content
      csvData = case allLines of
        [] -> CSVData [] []
        (h:rs) -> CSVData 
          { headers = splitOn (delimiter config) h
          , rows = map (splitOn (delimiter config)) rs
          }
  putStrLn $ "✓ Read " ++ show (length $ rows csvData) ++ " rows"
  return (config, csvData)

cleanData :: (CSVConfig, CSVData) -> IO (CSVConfig, CSVData)
cleanData (config, csvData) = do
  let cleaned = csvData
        { rows = map (map trim) (rows csvData)
        , headers = map trim (headers csvData)
        }
  putStrLn "✓ Cleaned data (trimmed whitespace)"
  return (config, cleaned)

analyzeData :: (CSVConfig, CSVData) -> IO (CSVConfig, Summary)
analyzeData (config, csvData) = do
  let summary = Summary
        { totalRows = length (rows csvData)
        , totalColumns = length (headers csvData)
        , emptyCells = countEmpty (rows csvData)
        }
  putStrLn $ "✓ Analysis complete: " ++ show (totalRows summary) ++ " rows"
  return (config, summary)

writeSummary :: (CSVConfig, Summary) -> IO Summary
writeSummary (config, summary) = do
  let output = unlines
        [ "CSV Analysis Summary"
        , "==================="
        , "Input file: " ++ inputPath config
        , "Total rows: " ++ show (totalRows summary)
        , "Total columns: " ++ show (totalColumns summary)
        , "Empty cells: " ++ show (emptyCells summary)
        , "Fill rate: " ++ show (fillRate summary) ++ "%"
        ]
  writeFile (outputPath config) output
  putStrLn $ "✓ Summary written to: " ++ outputPath config
  return summary

-- Helper functions
splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn delim str = 
  let (before, remainder) = break (== delim) str
  in before : case remainder of
    [] -> []
    (_:after) -> splitOn delim after

trim :: String -> String
trim = dropWhile Char.isSpace . reverse . dropWhile Char.isSpace . reverse

countEmpty :: [[String]] -> Int
countEmpty rows = sum [length (filter null row) | row <- rows]

fillRate :: Summary -> Double
fillRate summary = 
  let totalCells = totalRows summary * totalColumns summary
      filledCells = totalCells - emptyCells summary
  in if totalCells > 0
     then (fromIntegral filledCells / fromIntegral totalCells) * 100
     else 0

-- Pipeline with error handling
robustCSVPipeline :: Workflow CSVConfig (Either String Summary)
robustCSVPipeline = 
  (csvPipeline >>> step "success" (return . Right)) `catch`
  step "handle error" (\config -> return $ Left $ "Failed to process: " ++ inputPath config)

-- Parallel analysis pipeline
parallelAnalysis :: Workflow CSVConfig (Summary, Int)
parallelAnalysis = 
  csvPipeline &&&
  step "count lines" (\config -> do
    content <- readFile (inputPath config)
    return $ length (lines content))

-- Example usage
main :: IO ()
main = do
  putStrLn "=== CSV Processing Pipeline ==="
  
  -- Create sample CSV file
  let sampleCSV = unlines
        [ "Name,Age,City"
        , "Alice,30,New York"
        , "Bob,25,San Francisco"
        , "Charlie,,London"
        , "David,35,"
        , "Eve,28,Paris"
        ]
  writeFile "sample.csv" sampleCSV
  putStrLn "Created sample.csv"
  
  -- Configure pipeline
  let config = CSVConfig
        { inputPath = "sample.csv"
        , outputPath = "summary.txt"
        , delimiter = ','
        }
  
  -- Run basic pipeline
  putStrLn "\nRunning basic pipeline:"
  summary <- runWorkflow csvPipeline config
  putStrLn $ "\nResults: " ++ show summary
  
  -- Run robust pipeline
  putStrLn "\nRunning robust pipeline:"
  result <- runWorkflow robustCSVPipeline config
  case result of
    Right s -> putStrLn $ "Success: " ++ show s
    Left err -> putStrLn $ "Error: " ++ err
  
  -- Run parallel analysis
  putStrLn "\nRunning parallel analysis:"
  (summary2, lineCount) <- runWorkflow parallelAnalysis config
  putStrLn $ "Summary: " ++ show summary2
  putStrLn $ "Line count: " ++ show lineCount
  
  -- Try with non-existent file
  putStrLn "\nTrying with non-existent file:"
  let badConfig = config { inputPath = "nonexistent.csv" }
  badResult <- runWorkflow robustCSVPipeline badConfig
  case badResult of
    Right s -> putStrLn $ "Success: " ++ show s
    Left err -> putStrLn $ "Handled error: " ++ err
  
  putStrLn "\n✅ Pipeline demonstration complete!"