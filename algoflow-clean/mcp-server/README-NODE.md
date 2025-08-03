# AlgoFlow Node.js MCP Server

**Zero-dependency** MCP server implementation for the AlgoFlow workflow engine.

## Quick Start

```bash
# Run directly (no install needed!)
node algoflow-node.mjs

# Or make it executable
chmod +x algoflow-node.mjs
./algoflow-node.mjs
```

## Claude Desktop Setup

Add to your Claude Desktop config:

```json
{
  "mcpServers": {
    "algoflow": {
      "command": "node",
      "args": ["/absolute/path/to/algoflow-node.mjs"]
    }
  }
}
```

## Manual Testing

Run the test harness:
```bash
./test-node-server.mjs
```

## Available Tools

### 1. `create_workflow`
Create custom workflows with steps:
- `transform` - Transform data
- `filter` - Filter out null/undefined values
- `enrich` - Add metadata
- `double` - Double numeric values
- `concatenate` - Concatenate values

### 2. `execute_workflow`
Execute workflows with inputs. Predefined workflows:
- `simple` - Basic transformation
- `double` - Doubles numeric input
- `concatenate` - Concatenates input with itself
- `pipeline` - Multi-step pipeline (transform → enrich → filter)

### 3. `list_workflows`
List all available workflows (predefined + custom)

### 4. `workflow_status`
Check execution history (`all` or specific execution ID)

## Test Commands for Claude

1. "Use list_workflows to see what's available"
2. "Use execute_workflow with workflow_id 'double' and inputs 42"
3. "Create a workflow called 'test' with transform and enrich steps"
4. "Execute the 'test' workflow with inputs {name: 'Alice', value: 100}"
5. "Show workflow_status for execution_id 'all'"

## Architecture

- **Zero dependencies** - Just Node.js built-ins
- **State persistence** - Saves to `workflow-state.json`
- **Simple workflow engine** - ~300 lines total
- **MCP protocol** - JSON-RPC over stdio

## Extending

Add new step types in the `WorkflowEngine.execute()` switch:

```javascript
case 'uppercase':
  result = typeof result === 'string' ? result.toUpperCase() : result;
  break;
```

That's it! The Algorithm is simplicity itself. 🚀