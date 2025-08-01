# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Haskell-based workflow/computational graph engine that demonstrates clean, type-safe approaches to building workflow systems. The project contains two main implementations:

1. **AlgoFlow Clean** (`/algoflow-clean/`) - Comprehensive workflow engine with multiple DSL approaches
2. **Graph Runner** (`/algoflow-clean/v2/`) - Simplified ~150-line implementation focused on core concepts

## Build Commands

```bash
# Main project
cd algoflow-clean
cabal build              # Build the project
cabal run               # Run the main executable
cabal test              # Run tests (not yet implemented)

# V2 implementation
cd algoflow-clean/v2
cabal build             # Build the v2 project
cabal run demo          # Run the demo executable
```

## Development Commands

```bash
# Type checking and compilation
cabal build --ghc-options="-Wall"  # Build with all warnings

# REPL for interactive development
cabal repl              # Load modules in GHCi
cabal repl v2:demo      # Load v2 demo in GHCi

# Documentation
cabal haddock           # Generate Haddock documentation
```

## High-Level Architecture

### Core Design Principles
- **Type Safety First**: Uses GADTs, type families, and phantom types to make illegal states unrepresentable
- **Multiple Approaches**: Provides Arrow-based, Free monad, Graph-based, and Type-level implementations
- **Composability**: All components compose naturally using standard Haskell abstractions
- **Mathematical Correctness**: Implements proper Category and Arrow instances with law compliance

### Key Modules and Their Purpose

**Main Implementation (`/algoflow-clean/src/`)**:
- `Flow.Core.hs` - Arrow-based workflow abstraction using profunctors
- `Flow.Free.hs` - Free monad DSL for flexible, introspectable workflows
- `Flow.Graph.hs` - Graph representation using algebraic-graphs library
- `Flow.Typed.hs` - Type-level dependency checking at compile time
- `Flow.Execute.hs` - Asynchronous execution engine with parallel support
- `Flow.Cache.hs` - Caching interface (SQLite implementation planned)
- `Flow.Examples.hs` - Comprehensive usage examples

**V2 Implementation (`/algoflow-clean/v2/`)**:
- `Graph.hs` - Core computational graph abstraction in ~150 lines
- `Examples.hs` - Real-world usage patterns
- `Analysis.hs` - Static graph analysis capabilities
- `Verify.hs` - Category law verification

### Workflow Construction Patterns

The codebase supports multiple ways to construct workflows:

1. **Arrow Notation**: Using Haskell's arrow syntax for point-free composition
2. **Free Monad DSL**: Building workflows as data structures for interpretation
3. **Graph Construction**: Direct graph building with vertex and edge definitions
4. **Type-Level Programming**: Compile-time dependency validation

### Key Type Classes and Abstractions
- `WorkFlow` - Core workflow abstraction with dependency management
- `Cacheable` - Interface for cacheable computations
- `Flow` - Arrow-based workflow composition
- `Graph` - Computational graph representation

## Important Notes

- **No external linting tools configured** - Rely on GHC warnings (`-Wall` flag)
- **Tests not yet implemented** - Test framework (Hspec + QuickCheck) is configured but no tests written
- **Pure Cabal project** - No Stack configuration, use Cabal exclusively
- **GHC 9.4+ required** - Uses base ^>=4.17.0.0

## Common Development Tasks

When implementing new features:
1. Choose the appropriate abstraction (Arrow vs Free vs Graph)
2. Ensure type safety - prefer compile-time errors over runtime failures
3. Add examples to demonstrate usage
4. Use existing patterns from `Flow.Examples.hs` as reference
5. Maintain composability - new components should compose with existing ones

## Architectural Decisions

- **Why multiple approaches?** Different problem domains benefit from different abstractions
- **Why GADTs?** Type-safe workflow construction with compile-time guarantees
- **Why Free monads?** Introspectable workflows that can be analyzed before execution
- **Why algebraic-graphs?** Mathematical foundation for graph operations and analysis

## Learning Documentation

**IMPORTANT**: Update LESSONS.md with a "Stop and Jot" reflection at least once per pull request. Use the 3-2-1 format:
- 3 things you learned
- 2 highlights/interesting facts  
- 1 question you still have

This helps capture insights and learning throughout the development process. See LESSONS.md for the template and examples.

## Established Development Workflow

### 1. Project Organization
- **GitHub Issues**: All work is tracked through detailed GitHub issues with clear context and acceptance criteria
- **Umbrella Issues**: Complex features use umbrella issues with sub-task checkboxes
- **Labels**: Use descriptive labels (enhancement, documentation, testing, etc.)
- **Project Board**: Tasks organized in columns (Backlog → In Progress → Review → Done)

### 2. Development Process
1. **Start with an Issue**: Every change should have a corresponding issue
2. **Create Feature Branch**: Use naming convention (feature/, fix/, docs/, etc.)
3. **Implement Changes**: Follow code style guidelines in CONTRIBUTING.md
4. **Update Tests**: Add/modify tests as needed (test suite in progress)
5. **Update Documentation**: Keep docs in sync with code changes
6. **Update LESSONS.md**: Add stop-and-jot reflection
7. **Submit PR**: Use PR template, reference issues

### 3. CI/CD Pipeline
- **Build Matrix**: Tests run on GHC 9.4, 9.6, 9.8 across Ubuntu, macOS, Windows
- **HLint**: Static analysis checks in CI
- **Documentation**: Auto-deployed to GitHub Pages on main branch
- **Test Suite**: Basic tests run (comprehensive suite in development)

### 4. Key Files Created
- `.github/ISSUE_TEMPLATE/` - Bug, feature, and workflow example templates
- `.github/pull_request_template.md` - PR checklist including LESSONS.md
- `.github/workflows/ci.yml` - Main CI pipeline
- `.github/workflows/docs.yml` - Documentation deployment
- `.github/CODEOWNERS` - Code review assignments
- `CONTRIBUTING.md` - Development guidelines
- `LESSONS.md` - Learning documentation

### 5. Current Project State
- ✅ Infrastructure: CI/CD, issue templates, documentation
- ✅ Basic test harness with placeholder tests
- 🚧 Comprehensive test suite (Issue #11)
- 📋 PRD in progress (Issue #12)
- 📋 Core workflow examples planned
- 📋 Validation framework planned

### 6. Working with Issues
When picking up an issue:
1. Read the full issue description and context
2. Check linked issues and documentation
3. Review existing code patterns
4. Ask questions in issue comments if needed
5. Update issue status when starting work