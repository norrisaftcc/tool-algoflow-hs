{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.Types (parseMaybe, (.:))
import Data.Aeson.Key (fromText)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import GHC.Generics
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Data.ByteString.Lazy.Char8 as L8

import WorkflowBridge

-- MCP Protocol Types
data MCPRequest = MCPRequest
    { jsonrpc :: Text
    , method :: Text
    , params :: Value
    , id :: Maybe Value
    } deriving (Generic, Show)

instance FromJSON MCPRequest
instance ToJSON MCPRequest

data MCPResponse = MCPResponse
    { jsonrpc :: Text
    , result :: Maybe Value
    , error :: Maybe MCPError
    , id :: Maybe Value
    } deriving (Generic, Show)

instance ToJSON MCPResponse

data MCPError = MCPError
    { code :: Int
    , message :: Text
    , errorData :: Maybe Value
    } deriving (Generic, Show)

instance ToJSON MCPError where
    toJSON MCPError{..} = object
        [ "code" .= code
        , "message" .= message
        , "data" .= errorData
        ]

-- Tool definitions
data ToolDefinition = ToolDefinition
    { name :: Text
    , description :: Text
    , inputSchema :: Value
    } deriving (Generic, Show)

instance ToJSON ToolDefinition

-- Available tools
workflowTools :: [ToolDefinition]
workflowTools =
    [ ToolDefinition
        { name = "create_workflow"
        , description = "Create a new workflow from a specification"
        , inputSchema = object
            [ "type" .= ("object" :: Text)
            , "properties" .= object
                [ "name" .= object ["type" .= ("string" :: Text)]
                , "steps" .= object 
                    [ "type" .= ("array" :: Text)
                    , "items" .= object ["type" .= ("object" :: Text)]
                    ]
                ]
            , "required" .= ["name", "steps" :: Text]
            ]
        }
    , ToolDefinition
        { name = "execute_workflow"
        , description = "Execute a workflow with given inputs"
        , inputSchema = object
            [ "type" .= ("object" :: Text)
            , "properties" .= object
                [ "workflow_id" .= object ["type" .= ("string" :: Text)]
                , "inputs" .= object ["type" .= ("object" :: Text)]
                ]
            , "required" .= ["workflow_id" :: Text]
            ]
        }
    , ToolDefinition
        { name = "list_workflows"
        , description = "List all available workflows"
        , inputSchema = object
            [ "type" .= ("object" :: Text)
            , "properties" .= object []
            ]
        }
    ]

-- Server state
data ServerState = ServerState
    { workflows :: TVar [(Text, WorkflowSpec)]
    }

-- MCP request handler
handleMCPRequest :: ServerState -> MCPRequest -> IO MCPResponse
handleMCPRequest state req@MCPRequest{..} = case method of
    "initialize" -> return $ MCPResponse
        { jsonrpc = "2.0"
        , result = Just $ object
            [ "protocolVersion" .= ("1.0" :: Text)
            , "capabilities" .= object
                [ "tools" .= object ["available" .= True]
                ]
            , "serverInfo" .= object
                [ "name" .= ("AlgoFlow MCP Server" :: Text)
                , "version" .= ("0.1.0" :: Text)
                ]
            ]
        , error = Nothing
        , id = id
        }
    
    "tools/list" -> return $ MCPResponse
        { jsonrpc = "2.0"
        , result = Just $ object ["tools" .= workflowTools]
        , error = Nothing
        , id = id
        }
    
    "tools/call" -> handleToolCall state params id
    
    _ -> return $ MCPResponse
        { jsonrpc = "2.0"
        , result = Nothing
        , error = Just $ MCPError
            { code = -32601
            , message = "Method not found"
            , errorData = Nothing
            }
        , id = id
        }

-- Tool execution
handleToolCall :: ServerState -> Value -> Maybe Value -> IO MCPResponse
handleToolCall state params reqId = case params of
    Object obj -> case (,) <$> (Object obj) .! "name" <*> (Object obj) .! "arguments" of
        Just (String "create_workflow", args) -> do
            -- Simple workflow storage for now
            case args of
                Object argObj -> case decode (encode argObj) :: Maybe WorkflowSpec of
                    Just spec -> do
                        atomically $ modifyTVar (workflows state) ((workflowName spec, spec):)
                        return $ MCPResponse
                            { jsonrpc = "2.0"
                            , result = Just $ object
                                [ "content" .= [object ["type" .= ("text" :: Text), "text" .= ("Workflow created: " <> workflowName spec)]]
                                ]
                            , error = Nothing
                            , id = reqId
                            }
                    Nothing -> errorResponse "Invalid workflow specification" reqId
                _ -> errorResponse "Invalid arguments" reqId
        
        Just (String "list_workflows", _) -> do
            wfs <- readTVarIO (workflows state)
            let predefined = M.keys predefinedWorkflows
            let allWorkflows = map fst wfs ++ predefined
            return $ MCPResponse
                { jsonrpc = "2.0"
                , result = Just $ object
                    [ "content" .= [object 
                        [ "type" .= ("text" :: Text)
                        , "text" .= ("Available workflows:\n" <> 
                                    "Custom: " <> T.intercalate ", " (map fst wfs) <> "\n" <>
                                    "Predefined: " <> T.intercalate ", " predefined)
                        ]]
                    ]
                , error = Nothing
                , id = reqId
                }
        
        Just (String "execute_workflow", args) -> do
            case args of
                Object argObj -> case (,) <$> (Object argObj) .! "workflow_id" <*> (Object argObj) .! "inputs" of
                    Just (String wfId, inputs) -> do
                        -- Try predefined workflows first
                        if M.member wfId predefinedWorkflows
                            then case inputs of
                                Number n -> do
                                    result <- executePredefinedWorkflow wfId (round n)
                                    return $ MCPResponse
                                        { jsonrpc = "2.0"
                                        , result = Just $ object
                                            [ "content" .= [object 
                                                [ "type" .= ("text" :: Text)
                                                , "text" .= ("Workflow result: " <> T.pack (show result))
                                                ]]
                                            ]
                                        , error = Nothing
                                        , id = reqId
                                        }
                                _ -> errorResponse "Predefined workflows require numeric input" reqId
                            else do
                                -- Try custom workflows
                                wfs <- readTVarIO (workflows state)
                                case lookup wfId wfs of
                                    Just spec -> do
                                        result <- executeWorkflowFromSpec spec inputs
                                        return $ MCPResponse
                                            { jsonrpc = "2.0"
                                            , result = Just $ object
                                                [ "content" .= [object 
                                                    [ "type" .= ("text" :: Text)
                                                    , "text" .= ("Workflow result: " <> T.pack (show result))
                                                    ]]
                                                ]
                                            , error = Nothing
                                            , id = reqId
                                            }
                                    Nothing -> errorResponse "Workflow not found" reqId
                    _ -> errorResponse "Invalid execution arguments" reqId
                _ -> errorResponse "Invalid arguments" reqId
        
        _ -> errorResponse "Unknown tool" reqId
    _ -> errorResponse "Invalid params" reqId
  where
    errorResponse msg rid = return $ MCPResponse
        { jsonrpc = "2.0"
        , result = Nothing
        , error = Just $ MCPError
            { code = -32602
            , message = msg
            , errorData = Nothing
            }
        , id = rid
        }
    
    -- Helper to extract field from JSON object
    (.!) :: Value -> Text -> Maybe Value
    (Object obj) .! key = parseMaybe (.: fromText key) obj
    _ .! _ = Nothing

-- WAI Application
app :: ServerState -> Application
app state request respond = do
    body <- strictRequestBody request
    case decode body :: Maybe MCPRequest of
        Just mcpReq -> do
            response <- handleMCPRequest state mcpReq
            respond $ responseLBS
                status200
                [("Content-Type", "application/json")]
                (encode response)
        Nothing -> respond $ responseLBS
            status400
            [("Content-Type", "application/json")]
            (encode $ object ["error" .= ("Invalid JSON-RPC request" :: Text)])

main :: IO ()
main = do
    putStrLn "Starting AlgoFlow MCP Server on port 3000..."
    state <- ServerState <$> newTVarIO []
    run 3000 (app state)