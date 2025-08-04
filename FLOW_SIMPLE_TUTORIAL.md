# Flow.Simple Tutorial - Get Started in 5 Minutes

Welcome to AlgoFlow! This tutorial will get you building workflows in just 5 minutes using our simple, beginner-friendly API.

## Table of Contents
1. [Quick Start](#quick-start)
2. [Core Concepts](#core-concepts)
3. [Basic Examples](#basic-examples)
4. [Error Handling](#error-handling)
5. [Parallel Processing](#parallel-processing)
6. [Caching Results](#caching-results)
7. [Real-World Example](#real-world-example)
8. [Next Steps](#next-steps)

## Quick Start

### Installation

First, make sure you have the AlgoFlow library available:

```bash
cd algoflow-clean
cabal build
cabal repl
```

### Your First Workflow

Create a file called `MyFirstWorkflow.hs`:

```haskell
import Flow.Simple

-- Define a simple workflow
processFile :: Workflow String String
processFile = 
  step "read file" readFile' >>>
  step "uppercase" (return . map toUpper) >>>
  step "add header" addHeader
  where
    readFile' path = readFile path
    toUpper c = if c >= 'a' && c <= 'z' 
                then toEnum (fromEnum c - 32) 
                else c
    addHeader content = return $ "=== PROCESSED FILE ===\n" ++ content

-- Run it
main :: IO ()
main = do
  result <- runWorkflow processFile "input.txt"
  putStrLn result
```

That's it! You've just created a workflow that:
1. Reads a file
2. Converts it to uppercase
3. Adds a header

## Core Concepts

Flow.Simple has just 6 core functions you need to know:

### 1. `step` - Create a workflow step
```haskell
step :: String -> (a -> IO b) -> Workflow a b

-- Example
fetchUser :: Workflow Int User
fetchUser = step "fetch user from DB" $ \userId -> do
  -- database query here
  return (User userId "John Doe")
```

### 2. `>>>` - Run steps in sequence
```haskell
(>>>) :: Workflow a b -> Workflow b c -> Workflow a c

-- Example: Process data through multiple steps
processData :: Workflow String Int
processData = 
  step "parse" parseJSON >>>
  step "validate" validateData >>>
  step "count" countItems
```

### 3. `&&&` - Run steps in parallel
```haskell
(&&&) :: Workflow a b -> Workflow a c -> Workflow a (b, c)

-- Example: Fetch from two sources at once
fetchBoth :: Workflow UserId (Profile, Posts)
fetchBoth = 
  step "get profile" fetchProfile &&&
  step "get posts" fetchPosts
```

### 4. `catch` - Handle errors
```haskell
catch :: Workflow a b -> Workflow a b -> Workflow a b

-- Example: Provide fallback on error
safeOperation :: Workflow String String
safeOperation = 
  step "risky operation" riskyOp `catch`
  step "fallback" (\_ -> return "default value")
```

### 5. `cached` - Cache expensive operations
```haskell
cached :: String -> Workflow a b -> Workflow a b

-- Example: Cache expensive computation
fibonacci :: Workflow Int Int
fibonacci = cached "fib" $ 
  step "compute fibonacci" expensiveFib
```

### 6. `runWorkflow` - Execute a workflow
```haskell
runWorkflow :: Workflow a b -> a -> IO b

-- Example
main = do
  result <- runWorkflow myWorkflow "input"
  print result
```

## Basic Examples

### Example 1: File Processing Pipeline

```haskell
import Flow.Simple
import qualified Data.Char as Char

-- Count words in a file
wordCounter :: Workflow FilePath Int
wordCounter = 
  step "read file" readFile >>>
  step "count words" (return . length . words)

-- Usage
main = do
  count <- runWorkflow wordCounter "document.txt"
  putStrLn $ "Word count: " ++ show count
```

### Example 2: Data Transformation

```haskell
-- Transform user data
data User = User { name :: String, age :: Int }
data Profile = Profile { displayName :: String, category :: String }

transformUser :: Workflow User Profile
transformUser = 
  step "validate age" validateAge >>>
  step "create profile" createProfile
  where
    validateAge user = 
      if age user >= 18 
      then return user
      else error "User must be 18+"
    
    createProfile user = return $ Profile 
      { displayName = "User: " ++ name user
      , category = if age user >= 65 then "Senior" else "Regular"
      }
```

## Error Handling

### Basic Error Recovery

```haskell
-- Fetch with fallback
fetchDataSafely :: Workflow String String
fetchDataSafely = 
  step "fetch from API" fetchFromAPI `catch`
  step "return cached" (\_ -> return cachedData)
  where
    fetchFromAPI url = do
      -- This might fail
      response <- httpGet url
      return response
    
    cachedData = "Cached response"
```

### Custom Error Handling

```haskell
-- Handle specific errors
processWithLogging :: Workflow String String
processWithLogging = 
  step "process" process `catchWith` handleError
  where
    process input = do
      -- might throw exception
      result <- riskyOperation input
      return result
    
    handleError err input = do
      putStrLn $ "Error processing " ++ input ++ ": " ++ show err
      return "Error occurred"
```

## Parallel Processing

### Parallel Data Fetching

```haskell
-- Fetch user data from multiple sources
fetchUserData :: Workflow UserId (Profile, Posts, Friends)
fetchUserData = 
  (fetchProfile &&& fetchPosts) >>>
  step "add friends" addFriends
  where
    fetchProfile = step "get profile" $ \uid -> 
      -- fetch from profile service
    
    fetchPosts = step "get posts" $ \uid ->
      -- fetch from posts service
    
    addFriends (profile, posts) = do
      friends <- -- fetch friends based on profile
      return (profile, posts, friends)
```

### Parallel File Processing

```haskell
-- Process multiple files in parallel
processFiles :: Workflow (FilePath, FilePath) (String, String)
processFiles = 
  step "read file 1" (readFile . fst) &&&
  step "read file 2" (readFile . snd)

-- Usage
main = do
  (content1, content2) <- runWorkflow processFiles ("file1.txt", "file2.txt")
  putStrLn $ "Processed " ++ show (length content1 + length content2) ++ " characters"
```

## Caching Results

### Basic Caching

```haskell
-- Cache expensive computations
expensiveAnalysis :: Workflow String AnalysisResult
expensiveAnalysis = cached "analysis-v1" $
  step "analyze" $ \input -> do
    putStrLn "Running expensive analysis..."
    -- Simulate expensive computation
    threadDelay 2000000  -- 2 seconds
    return $ AnalysisResult (length input)

-- First call takes 2 seconds, subsequent calls are instant
main = do
  result1 <- runWorkflow expensiveAnalysis "test"  -- Slow
  result2 <- runWorkflow expensiveAnalysis "test"  -- Fast (cached)
```

### Cache Key Strategies

```haskell
-- Different cache keys for different inputs
userReport :: Workflow UserId Report
userReport = cached "user-report" $
  step "generate report" $ \userId -> do
    -- This will cache per userId
    profile <- fetchUserProfile userId
    posts <- fetchUserPosts userId
    return $ generateReport profile posts
```

## Real-World Example

Here's a complete example of a data processing pipeline:

```haskell
import Flow.Simple
import qualified Data.Char as Char
import System.Directory (doesFileExist)

-- A real data processing pipeline
data Config = Config 
  { inputFile :: FilePath
  , outputFile :: FilePath
  , uppercase :: Bool
  }

-- Main processing pipeline
processDataPipeline :: Workflow Config ()
processDataPipeline = 
  step "validate config" validateConfig >>>
  step "read input" readInput >>>
  step "transform" transform >>>
  step "write output" writeOutput
  where
    validateConfig config = do
      exists <- doesFileExist (inputFile config)
      if exists 
        then return config
        else error $ "Input file not found: " ++ inputFile config
    
    readInput config = do
      content <- readFile (inputFile config)
      return (config, content)
    
    transform (config, content) = 
      let transformed = if uppercase config
                        then map Char.toUpper content
                        else content
      in return (config, transformed)
    
    writeOutput (config, content) = do
      writeFile (outputFile config) content
      putStrLn $ "Processed " ++ inputFile config ++ " -> " ++ outputFile config

-- With error handling and caching
robustPipeline :: Workflow Config ()
robustPipeline = 
  processDataPipeline `catch`
  step "report error" reportError
  where
    reportError config = do
      putStrLn $ "Failed to process " ++ inputFile config
      putStrLn "Using default output"
      writeFile (outputFile config) "ERROR: Processing failed"

-- Usage
main :: IO ()
main = do
  let config = Config 
        { inputFile = "data.txt"
        , outputFile = "output.txt"
        , uppercase = True
        }
  
  runWorkflow robustPipeline config
  putStrLn "Pipeline completed!"
```

## Debugging Workflows

### Adding Logging

```haskell
-- Add logging to any step
withLogging :: String -> (a -> IO b) -> (a -> IO b)
withLogging name action = \input -> do
  putStrLn $ "[" ++ name ++ "] Starting..."
  result <- action input
  putStrLn $ "[" ++ name ++ "] Completed"
  return result

-- Use it
debugWorkflow :: Workflow String String
debugWorkflow = 
  step "step1" (withLogging "step1" process1) >>>
  step "step2" (withLogging "step2" process2)
```

### Inspecting Intermediate Values

```haskell
-- Debug helper
debug :: Show a => String -> Workflow a a
debug label = step ("debug: " ++ label) $ \x -> do
  putStrLn $ label ++ ": " ++ show x
  return x

-- Use in pipeline
pipeline :: Workflow Int String
pipeline = 
  step "double" (\x -> return (x * 2)) >>>
  debug "after double" >>>
  step "stringify" (return . show) >>>
  debug "final result"
```

## Best Practices

### 1. Name Your Steps
Always give descriptive names to your steps:
```haskell
-- Good
step "fetch user profile" fetchProfile

-- Bad  
step "step1" fetchProfile
```

### 2. Keep Steps Focused
Each step should do one thing:
```haskell
-- Good
parseJSON >>> validateData >>> saveToDb

-- Bad
step "do everything" $ \input -> do
  parsed <- parseJSON input
  validated <- validate parsed
  saveToDb validated
```

### 3. Use Types
Let types guide your workflow:
```haskell
type Username = String
type UserId = Int
type Profile = (UserId, String, Int)

fetchUserId :: Workflow Username UserId
fetchProfile :: Workflow UserId Profile
```

### 4. Handle Errors Early
```haskell
processUser :: Workflow String User
processUser = 
  step "validate input" validateInput >>>  -- Fail fast
  step "parse user" parseUser >>>
  step "enrich data" enrichData
```

## Next Steps

### When You're Ready for More

Once you're comfortable with Flow.Simple, you can:

1. **Explore advanced features** in Flow.Core:
   - Arrow notation
   - Custom combinators
   - Advanced caching strategies

2. **Try other workflow styles**:
   - Flow.Free for introspectable workflows
   - Flow.Graph for dependency graphs

3. **Build complex systems**:
   - Microservice orchestration
   - Data pipeline systems
   - Build automation

### Getting Help

- Check the examples in `test/Flow/SimpleSpec.hs`
- Read the API documentation
- Ask questions on GitHub issues

## Summary

You now know everything you need to build workflows with Flow.Simple:

- `step` - Create workflow steps
- `>>>` - Sequential composition
- `&&&` - Parallel composition  
- `catch` - Error handling
- `cached` - Result caching
- `runWorkflow` - Execute workflows

Start simple, and grow your workflows as your needs evolve. Happy workflow building!