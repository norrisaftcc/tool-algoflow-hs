# How to Bootstrap AlgoFlow Development Environment

This guide helps you quickly set up and start working with the AlgoFlow project, including the MCP server integration.

## Prerequisites

- **Haskell**: GHC 9.4+ and Cabal (for the core engine)
- **Node.js**: v18+ (for the MCP server)
- **Claude Code**: Latest version with MCP support
- **Git**: For version control

## Quick Start

### 1. Clone and Build the Project

```bash
# Clone the repository
git clone https://github.com/norrisaftcc/tool-algoflow-hs.git
cd tool-algoflow-hs/algoflow-clean

# Build the Haskell project
cabal update
cabal build

# Run tests to verify everything works
cabal test
```

### 2. Set Up MCP Server for Claude Code

The project includes an MCP (Model Context Protocol) server that allows Claude to interact with the AlgoFlow workflow engine.

**Option A: Use Claude Code with MCP** (Recommended)
1. Open Claude Code in the project root directory
2. The `claude-code.json` file is already configured
3. Restart Claude Code - it will automatically start the MCP server
4. You'll have access to these MCP tools:
   - `create_workflow` - Create custom workflows
   - `execute_workflow` - Run workflows with inputs
   - `list_workflows` - See available workflows
   - `workflow_status` - Check execution history

**Option B: Test MCP Server Manually**
```bash
# Test the Node.js MCP server
cd algoflow-clean/mcp-server
node algoflow-node.mjs

# In another terminal, run the test harness
node test-node-server.mjs
```

**Option C: Use Haskell MCP Server**
```bash
# Build and run the Haskell MCP server
cabal build algoflow-mcp-server
cabal run algoflow-mcp-server
```

## Project Structure

```
tool-algoflow-hs/
├── algoflow-clean/          # Main Haskell project
│   ├── src/                 # Source code
│   │   ├── Flow/Core.hs    # Arrow-based workflows
│   │   ├── Flow/Free.hs    # Free monad DSL
│   │   ├── Flow/Graph.hs   # Graph representation
│   │   └── Flow/Execute.hs # Execution engine
│   ├── test/               # Test suite
│   └── mcp-server/         # MCP server implementations
│       ├── algoflow-node.mjs    # Node.js server (zero deps)
│       └── Main.hs              # Haskell server
├── claude-code.json        # MCP configuration for Claude Code
├── CLAUDE.md              # Instructions for Claude
├── LESSONS.md             # Learning journal
└── SPRINT_*.md            # Sprint planning docs
```

## Development Workflow

### Starting a New Feature

1. **Check Current Sprint**
   ```bash
   # See what we're working on
   cat SPRINT_2_PLAN.md
   ```

2. **Pick a Task**
   - High-priority tasks are listed first
   - Check GitHub issues for more context

3. **Use Claude Code**
   - Claude has access to CLAUDE.md for project context
   - Use TodoWrite tool to track progress
   - Update LESSONS.md with insights

### Running Tests

```bash
# Run all tests
cabal test

# Run with details on failure
cabal test --test-show-details=failures

# Run specific test suite
cabal test algoflow-test
```

### Working with MCP Tools

When Claude Code is restarted with this project:

1. **Create a Workflow**
   ```
   Use create_workflow tool to define a new workflow with steps like:
   - transform
   - filter
   - enrich
   - validate
   ```

2. **Execute Workflows**
   ```
   Use execute_workflow with the workflow name and input data
   ```

3. **Check Status**
   ```
   Use workflow_status to see execution history
   ```

## Common Tasks

### Adding a New Workflow Type

1. Edit `algoflow-clean/src/Flow/Core.hs` or `Flow/Free.hs`
2. Add your workflow logic
3. Update tests in `test/`
4. If needed, update MCP server to expose it

### Debugging MCP Server

```bash
# Check if server is running
ps aux | grep algoflow-node

# View server logs (if configured)
tail -f workflow-state.json

# Test specific MCP commands
node test-node-server.mjs
```

### Updating Dependencies

```bash
# Haskell dependencies
cabal update
cabal build --dependencies-only

# The Node.js MCP server has zero dependencies!
```

## Troubleshooting

### "Package list does not exist" Error
```bash
rm -rf ~/.cabal/packages
cabal update
```

### MCP Server Not Available in Claude
1. Ensure `claude-code.json` exists in project root
2. Restart Claude Code completely
3. Check the command path in claude-code.json is absolute

### Type Errors in Haskell Code
- Check LESSONS.md for similar issues we've solved
- Use `:type` in `cabal repl` to inspect types
- Remember: TypeApplications need explicit `forall`

## Important Notes

- **All tests must pass** before committing (currently 74/74 ✅)
- **Update LESSONS.md** with 3-2-1 reflections
- **No external linting** - use GHC's `-Wall` flag
- **MCP server** is for Claude integration, not required for core development

## Next Steps

1. Review `SPRINT_2_PLAN.md` for current priorities
2. Check GitHub issues for detailed task descriptions
3. Read `CLAUDE.md` for coding guidelines
4. Start with SQLite caching if you're looking for a meaty task!

## Getting Help

- **GitHub Issues**: Best place for questions
- **LESSONS.md**: Check if someone hit the same issue
- **Test Suite**: Great examples of how to use the API

---

Remember: We're glad we stuck with Haskell! The type safety has paid off tremendously. 🎉