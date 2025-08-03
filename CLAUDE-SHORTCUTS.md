# CLAUDE-SHORTCUTS.md

## Quick Reference for Working with AlgoFlow Haskell Project

### 🚀 Type Error Shortcuts

#### When you see "No instance for (Category/Arrow) for Workflow"
```haskell
-- WRONG: Workflow doesn't have Arrow/Category instances
workflow = w1 >>> w2
workflow = w1 *** w2

-- RIGHT: Use explicit constructors
workflow = Seq w1 w2
workflow = Par w1 w2
```

#### Free Monad Workflow Composition
```haskell
-- WRONG: Can't bind workflows that produce different types
combined = do
  i <- intWorkflow    -- Workflow Int Int
  stringWorkflow      -- Workflow Int String

-- RIGHT: Use a single workflow
combined = compute "convert" (\x -> return (show (x + 1)))
```

### 🔍 Quick Search Commands

```bash
# Find type definitions
rg "^data.*ExecutionResult"
rg "^data.*Error"

# Find instances
rg "instance.*Category.*Workflow"

# Check exports with context
rg -A5 "module Flow.Cache"

# Find constructor usage
rg "ExecutionResult.*->"
```

### 📝 Test Patterns

#### ExecutionResult Pattern Matching
```haskell
-- ExecutionResult has 3 fields (not 4!)
case result of
  Right (ExecutionResult value duration cacheHit) -> ...
  -- NOT: Right (ExecutionResult value _ _ _)
```

#### CacheSpec withCache Pattern
```haskell
-- withCache handles serialization internally
-- Computation should return domain type, not ByteString
let expensive x = return (sum [1..x] :: Int)  -- RIGHT
-- NOT: let expensive x = return $ BS8.pack $ show (sum [1..x])
```

### 🛠️ Common Fixes

#### Import Text type for FreeSpec
```haskell
import Data.Text (Text)
import qualified Data.Text as T

-- Then use:
T.unpack dry_output `shouldContain` "step1"
```

#### Type Annotations for Ambiguous Tests
```haskell
-- Add explicit type annotations to avoid defaulting warnings
let wf1 = compute "branch1" (\x -> return (x * 2 :: Int))
    combined :: Workflow (Int, Int) Int
```

### 🎯 Cabal Commands

```bash
# Quick test runs
cabal test --test-show-details=streaming  # See output as tests run
cabal test --test-show-details=failures   # Only show failures

# Build with all warnings
cabal build --ghc-options="-Wall"

# Quick REPL for testing
cabal repl test:algoflow-test
```

### 🔧 Module-Specific Tips

#### Flow.Execute
- ExecutionError constructors: `StepFailed Text SomeException`, `CacheError Text`
- ExecutionConfig fields: `maxParallel`, `enableCache`, `cacheDir`
- Default config has `enableCache = True` (not False as tests expect)

#### Flow.Free
- `dryRun` returns `Text`, not `String`
- Parallel workflows return tuples, can't easily bind the result
- Use `compute` to create workflow steps

#### Flow.Core
- Workflow is a GADT, not an Arrow/Category
- Use `Seq`, `Par`, `Step`, `Cache`, `Recover` constructors
- `interpret` converts Workflow to Flow

### 📋 Test Failure Patterns

1. **Cache hit counting off by one**: Check if first access counts as hit
2. **Complex workflow math wrong**: Trace through sequential vs parallel execution
3. **dryRun output case sensitive**: "Parallel" vs "parallel"
4. **Error not caught**: Check if error happens during construction vs execution

### 💡 Pro Tips

1. **Always check what the actual API exports**:
   ```bash
   rg "module.*Flow.Cache" -A20 | grep -E "^\s+,"
   ```

2. **When tests expect specific constructors**:
   ```bash
   rg "ExecutionError\(" src/
   ```

3. **For type mismatches, check the actual type signature**:
   ```bash
   rg "withCache ::" src/
   ```

4. **Use MultiEdit for multiple similar changes** instead of multiple Edit calls

5. **Run focused tests during debugging**:
   ```bash
   cabal test --test-options="--match '/Flow.Cache/'"
   ```

### 🐛 Common Gotchas

- `Flow` has Arrow instance, `Workflow` doesn't
- `withCache` does serialization for you - don't serialize in your computation
- Test frameworks might shadow names (e.g., `parallel` from Test.Hspec)
- ExecutionResult changed from 4 fields to 3 fields at some point
- Default ExecutionConfig values might not match test expectations

### 📚 Quick Module Reference

```
Flow.Core       - Arrow-based Flow, GADT Workflow
Flow.Free       - Free monad workflows, compute/parallel/cache/recover
Flow.Execute    - Async execution, ExecutionConfig, ExecutionResult
Flow.Cache      - Cache interface, mkCacheKey, withCache
Flow.Graph      - Graph-based workflows using algebraic-graphs
Flow.Error      - Error types and retry policies
```

Remember: When in doubt, read the actual module exports and type signatures!