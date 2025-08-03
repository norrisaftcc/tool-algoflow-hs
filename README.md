# AlgoFlow

[![CI](https://github.com/norrisaftcc/tool-algoflow-hs/actions/workflows/ci.yml/badge.svg)](https://github.com/norrisaftcc/tool-algoflow-hs/actions/workflows/ci.yml)
[![Documentation](https://github.com/norrisaftcc/tool-algoflow-hs/actions/workflows/docs.yml/badge.svg)](https://github.com/norrisaftcc/tool-algoflow-hs/actions/workflows/docs.yml)
[![Haskell](https://img.shields.io/badge/language-Haskell-5e35b1.svg)](https://www.haskell.org)
[![License](https://img.shields.io/github/license/norrisaftcc/tool-algoflow-hs)](LICENSE)

A type-safe workflow engine for Haskell that demonstrates clean, composable approaches to building computational graphs and data pipelines.

## Features

- **Multiple DSL Approaches**: Choose between Arrow-based, Free monad, Graph-based, or Type-level implementations
- **Type Safety First**: Leverage Haskell's type system to catch errors at compile time
- **Parallel Execution**: Built-in support for concurrent workflow execution
- **Mathematical Correctness**: Proper Category and Arrow law implementations
- **Clean Architecture**: Learn from both comprehensive and minimal implementations

## Quick Start

### Prerequisites

- GHC 9.4 or later
- Cabal 3.0 or later

### Installation

```bash
# Clone the repository
git clone https://github.com/norrisaftcc/tool-algoflow-hs.git
cd tool-algoflow-hs

# Build the main project
cd algoflow-clean
cabal build

# Run examples
cabal run

# Or try the simplified v2 implementation
cd v2
cabal run demo
```

## Project Structure

```
tool-algoflow-hs/
├── algoflow-clean/          # Main implementation
│   ├── src/                 # Core library modules
│   ├── test/               # Test suite (in progress)
│   └── algoflow-clean.cabal
├── algoflow-clean/v2/       # Simplified ~150 line implementation
│   ├── Graph.hs            # Core computational graph
│   ├── Examples.hs         # Usage examples
│   └── graph-runner.cabal
└── docs/                   # Documentation and design notes
```

## Usage Examples

### Arrow-Based Workflow

```haskell
import Flow.Core

-- Define a simple data processing workflow
myWorkflow :: Flow String Int
myWorkflow = proc input -> do
    cleaned <- cleanData -< input
    parsed <- parseData -< cleaned
    result <- calculate -< parsed
    returnA -< result
```

### Free Monad DSL

```haskell
import Flow.Free

-- Build workflows as data structures
myPipeline :: WorkflowF String Int
myPipeline = do
    x <- step "clean" cleanData
    y <- step "parse" parseData  
    z <- step "calc" calculate
    return z
```

### Graph Construction

```haskell
import Flow.Graph

-- Direct graph building
myGraph :: Graph Task
myGraph = overlay (vertex task1) (vertex task2)
       `connect` vertex task3
```

## Documentation

- [Design Documentation](DESIGN.md) - Architectural decisions and rationale
- [API Documentation](https://norrisaftcc.github.io/tool-algoflow-hs/) - Generated Haddock docs
- [Examples](algoflow-clean/src/Flow/Examples.hs) - Comprehensive usage examples
- [Contributing Guidelines](CONTRIBUTING.md) - How to contribute

## Why AlgoFlow?

AlgoFlow was created to demonstrate how Haskell's type system and functional programming paradigms can create safer, more maintainable workflow engines compared to traditional approaches. It serves both as a practical tool and an educational resource.

### Key Benefits

1. **Compile-Time Safety**: Many workflow errors are caught before runtime
2. **Composability**: Workflows compose like functions
3. **Multiple Paradigms**: Choose the approach that fits your problem
4. **Learning Resource**: Understand functional programming patterns in practice

## Roadmap

- [x] Core workflow engine implementations
- [x] CI/CD pipeline
- [ ] Comprehensive test suite
- [ ] Workflow visualization (Graphviz)
- [ ] Distributed execution support
- [ ] Performance benchmarks

## Contributing

We welcome contributions! Please see our [Contributing Guidelines](CONTRIBUTING.md) for details on:
- Code style and standards
- Development workflow
- Issue reporting
- Pull request process

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

This project demonstrates clean Haskell patterns as an alternative to enterprise-style implementations. Special thanks to the Haskell community for inspiration and support.

---

For more information, visit our [documentation site](https://norrisaftcc.github.io/tool-algoflow-hs/) or check out the [issues](https://github.com/norrisaftcc/tool-algoflow-hs/issues) for ways to contribute.