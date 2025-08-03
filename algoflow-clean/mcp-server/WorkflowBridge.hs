{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module WorkflowBridge where

import Control.Monad
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Generics

-- Import from AlgoFlow
import Flow.Core
import Flow.Execute
import Flow.Free
-- import Flow.Example (exampleFlow1, exampleFlow2)  -- Disabled temporarily

-- JSON representations
data WorkflowSpec = WorkflowSpec
    { workflowName :: Text
    , workflowSteps :: [StepSpec]
    } deriving (Generic, Show)

instance FromJSON WorkflowSpec
instance ToJSON WorkflowSpec

data StepSpec = StepSpec
    { stepName :: Text
    , stepType :: Text
    , stepInputs :: Map Text Value
    , stepDependencies :: [Text]
    } deriving (Generic, Show)

instance FromJSON StepSpec
instance ToJSON StepSpec

-- Workflow execution result
data WorkflowResult = WorkflowResult
    { resultStatus :: Text
    , resultValue :: Maybe Value
    , resultError :: Maybe Text
    , executionTime :: Double
    } deriving (Generic, Show)

instance ToJSON WorkflowResult

-- Pre-defined workflows (as a quick kludge)
predefinedWorkflows :: Map Text (Flow Int String)
predefinedWorkflows = M.fromList
    [ -- ("example1", exampleFlow1)  -- Disabled temporarily
      -- , ("example2", exampleFlow2)  -- Disabled temporarily
      ("simple", arr (\x -> "Input: " ++ show x))
    , ("double", arr (*2) >>> arr show)
    , ("concatenate", arr (\x -> show x) &&& arr (\x -> " doubled is " ++ show (x*2)) >>> arr (uncurry (++)))
    ]

-- Execute a predefined workflow
executePredefinedWorkflow :: Text -> Int -> IO WorkflowResult
executePredefinedWorkflow name input = case M.lookup name predefinedWorkflows of
    Just flow -> do
        let config = ExecutionConfig
                { maxParallel = 4
                , enableCaching = False
                , timeout = Nothing
                }
        result <- runWorkflow config flow input
        case result of
            Right (ExecutionResult value _ duration _) -> return WorkflowResult
                { resultStatus = "success"
                , resultValue = Just (toJSON value)
                , resultError = Nothing
                , executionTime = duration
                }
            Left err -> return WorkflowResult
                { resultStatus = "error"
                , resultValue = Nothing
                , resultError = Just (T.pack $ show err)
                , executionTime = 0
                }
    Nothing -> return WorkflowResult
        { resultStatus = "error"
        , resultValue = Nothing
        , resultError = Just "Workflow not found"
        , executionTime = 0
        }

-- Create a simple workflow from spec (very basic for now)
createSimpleWorkflow :: WorkflowSpec -> Maybe (Flow Value Value)
createSimpleWorkflow spec = case workflowSteps spec of
    [] -> Nothing
    [step] -> Just $ createStep step
    steps -> Just $ foldr1 (>>>) (map createStep steps)
  where
    createStep :: StepSpec -> Flow Value Value
    createStep StepSpec{..} = case stepType of
        "transform" -> arr $ \input -> object
            [ "step" .= stepName
            , "input" .= input
            , "transformed" .= True
            ]
        "filter" -> arr $ \input -> case input of
            Object o -> Object (M.filter (/= Null) (toMap o))
            v -> v
        "enrich" -> arr $ \input -> case input of
            Object o -> Object (M.insert "enriched" (Bool True) (toMap o))
            v -> object ["original" .= v, "enriched" .= True]
        _ -> arr id
    
    toMap :: Object -> Map Text Value
    toMap = id  -- In newer aeson versions, Object is already a Map

-- Execute workflow from JSON spec
executeWorkflowFromSpec :: WorkflowSpec -> Value -> IO WorkflowResult
executeWorkflowFromSpec spec input = case createSimpleWorkflow spec of
    Just flow -> do
        let config = ExecutionConfig
                { maxParallel = 4
                , enableCaching = False
                , timeout = Nothing
                }
        result <- runWorkflow config flow input
        case result of
            Right (ExecutionResult value _ duration _) -> return WorkflowResult
                { resultStatus = "success"
                , resultValue = Just value
                , resultError = Nothing
                , executionTime = duration
                }
            Left err -> return WorkflowResult
                { resultStatus = "error"
                , resultValue = Nothing
                , resultError = Just (T.pack $ show err)
                , executionTime = 0
                }
    Nothing -> return WorkflowResult
        { resultStatus = "error"
        , resultValue = Nothing
        , resultError = Just "Invalid workflow specification"
        , executionTime = 0
        }