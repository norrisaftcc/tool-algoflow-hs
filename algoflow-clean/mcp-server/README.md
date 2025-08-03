# AlgoFlow MCP Server

This is a Model Context Protocol (MCP) server that exposes the AlgoFlow workflow engine as MCP tools.

## Quick Start

1. Install Haskell toolchain (GHC 9.4+ and Cabal)
2. Build the server:
   ```bash
   cd algoflow-clean
   cabal build algoflow-mcp-server
   ```
3. Run the server:
   ```bash
   cabal run algoflow-mcp-server
   ```

The server will start on port 3000.

## Available Tools

### 1. `create_workflow`
Create a new workflow from a specification.

Example request:
```json
{
  "jsonrpc": "2.0",
  "method": "tools/call",
  "params": {
    "name": "create_workflow",
    "arguments": {
      "workflowName": "data-pipeline",
      "workflowSteps": [
        {
          "stepName": "transform-data",
          "stepType": "transform",
          "stepInputs": {},
          "stepDependencies": []
        },
        {
          "stepName": "enrich-data",
          "stepType": "enrich",
          "stepInputs": {},
          "stepDependencies": ["transform-data"]
        }
      ]
    }
  },
  "id": 1
}
```

### 2. `execute_workflow`
Execute a workflow with given inputs.

Predefined workflows available:
- `example1`, `example2` - Example workflows from Flow.Example
- `simple` - Simple string transformation
- `double` - Doubles numeric input
- `concatenate` - Concatenates input with its doubled value

Example request:
```json
{
  "jsonrpc": "2.0",
  "method": "tools/call",
  "params": {
    "name": "execute_workflow",
    "arguments": {
      "workflow_id": "double",
      "inputs": 42
    }
  },
  "id": 2
}
```

### 3. `list_workflows`
List all available workflows (both custom and predefined).

Example request:
```json
{
  "jsonrpc": "2.0",
  "method": "tools/call",
  "params": {
    "name": "list_workflows",
    "arguments": {}
  },
  "id": 3
}
```

## Testing with curl

Initialize the server:
```bash
curl -X POST http://localhost:3000 \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","method":"initialize","params":{},"id":1}'
```

List available tools:
```bash
curl -X POST http://localhost:3000 \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","method":"tools/list","params":{},"id":2}'
```

Execute a predefined workflow:
```bash
curl -X POST http://localhost:3000 \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc":"2.0",
    "method":"tools/call",
    "params":{
      "name":"execute_workflow",
      "arguments":{"workflow_id":"double","inputs":21}
    },
    "id":3
  }'
```

## Architecture

- **Main.hs**: MCP protocol implementation and HTTP server
- **WorkflowBridge.hs**: Bridge between MCP and AlgoFlow engine
- Uses the AlgoFlow workflow engine from the main library
- Supports both predefined and dynamically created workflows

## Future Enhancements

- Persistent workflow storage
- More sophisticated workflow composition
- Real-time workflow execution monitoring
- Support for streaming results
- Integration with Flow.Cache for result caching