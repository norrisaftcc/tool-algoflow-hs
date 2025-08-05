{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module WorkflowBridge where

import Control.Arrow
import Control.Monad
import Data.Aeson
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Generics

-- Import from AlgoFlow
import Flow.Core
import Flow.Execute
import qualified Flow.Free as Free
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
predefinedWorkflows :: Map Text (Workflow IO Int String)
predefinedWorkflows = M.fromList
    [ -- ("example1", exampleFlow1)  -- Disabled temporarily
      -- , ("example2", exampleFlow2)  -- Disabled temporarily
      ("simple", step "simple" (\x -> return $ "Input: " ++ show x))
    , ("double", step "multiply" (\x -> return (x * 2)) `Seq` step "show" (\x -> return $ show x))
    , ("concatenate", step "concatenate" (\x -> return $ show x ++ " doubled is " ++ show (x*2)))
    ]

-- Execute a predefined workflow
executePredefinedWorkflow :: Text -> Int -> IO WorkflowResult
executePredefinedWorkflow name input = case M.lookup name predefinedWorkflows of
    Just workflow -> do
        let config = ExecutionConfig
                { maxParallel = 4
                , enableCache = False
                , cacheDir = Nothing
                }
        result <- Flow.Execute.runWorkflow config workflow input
        case result of
            Right (ExecutionResult value duration _) -> return WorkflowResult
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
createSimpleWorkflow :: WorkflowSpec -> Maybe (Workflow IO Value Value)
createSimpleWorkflow spec = case workflowSteps spec of
    [] -> Nothing
    [singleStep] -> Just $ createStep singleStep
    steps -> Just $ foldr1 Seq (map createStep steps)
  where
    createStep :: StepSpec -> Workflow IO Value Value
    createStep StepSpec{..} = case stepType of
        "transform" -> step stepName $ \input -> return $ object
            [ "step" .= stepName
            , "input" .= input
            , "transformed" .= True
            ]
        "filter" -> step stepName $ \input -> return $ case input of
            Object o -> Object (KM.filter (/= Null) o)
            v -> v
        "enrich" -> step stepName $ \input -> return $ case input of
            Object o -> Object (KM.insert "enriched" (Bool True) o)
            v -> object ["original" .= v, "enriched" .= True]
        _ -> step stepName $ \input -> return input
    
    -- Note: Object is now a KeyMap in newer Aeson versions, not a Map

-- Execute workflow from JSON spec
executeWorkflowFromSpec :: WorkflowSpec -> Value -> IO WorkflowResult
executeWorkflowFromSpec spec input = case createSimpleWorkflow spec of
    Just workflow -> do
        let config = ExecutionConfig
                { maxParallel = 4
                , enableCache = False
                , cacheDir = Nothing
                }
        result <- Flow.Execute.runWorkflow config workflow input
        case result of
            Right (ExecutionResult value duration _) -> return WorkflowResult
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