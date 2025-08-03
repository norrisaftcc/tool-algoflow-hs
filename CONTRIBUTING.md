# Contributing to AlgoFlow

Thank you for your interest in contributing to AlgoFlow! This document provides guidelines and instructions for contributing to the project.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
- [Development Process](#development-process)
- [Code Style](#code-style)
- [Testing Guidelines](#testing-guidelines)
- [Documentation](#documentation)
- [Submitting Changes](#submitting-changes)
- [Learning Documentation](#learning-documentation)

## Code of Conduct

We are committed to providing a welcoming and inclusive environment. Please be respectful and constructive in all interactions.

## Getting Started

### Prerequisites

- GHC 9.4.7 or later (we test against 9.4, 9.6, and 9.8)
- Cabal 3.10 or later
- Git
- GitHub account

### Setting Up Your Development Environment

1. Fork the repository on GitHub
2. Clone your fork:
   ```bash
   git clone https://github.com/YOUR-USERNAME/tool-algoflow-hs.git
   cd tool-algoflow-hs
   ```

3. Add the upstream remote:
   ```bash
   git remote add upstream https://github.com/norrisaftcc/tool-algoflow-hs.git
   ```

4. Build the project:
   ```bash
   cd algoflow-clean
   cabal build
   cabal test
   ```

## Development Process

### Workflow

1. **Check existing issues** or create a new one to discuss your proposed changes
2. **Create a feature branch** from `main`:
   ```bash
   git checkout -b feature/your-feature-name
   ```
3. **Make your changes** following our code style guidelines
4. **Write or update tests** for your changes
5. **Update documentation** as needed
6. **Update LESSONS.md** with your learning insights (see below)
7. **Submit a pull request**

### Branch Naming Conventions

- `feature/` - New features
- `fix/` - Bug fixes
- `docs/` - Documentation only changes
- `refactor/` - Code refactoring
- `test/` - Test additions or fixes

## Code Style

### Haskell Style Guide

We follow these conventions:

1. **Formatting**
   - Use 2 spaces for indentation (no tabs)
   - Maximum line length: 80 characters
   - Use explicit imports where practical

2. **Naming**
   - Use camelCase for functions and variables
   - Use PascalCase for types and constructors
   - Descriptive names preferred over abbreviations

3. **Type Signatures**
   - Always include type signatures for top-level functions
   - Use explicit forall for complex types
   - Document type parameters

4. **Documentation**
   - Use Haddock comments for all exported functions
   - Include examples in documentation
   - Explain "why" not just "what"

### Example

```haskell
-- | Execute a workflow with the given input.
-- 
-- This function handles parallel execution of workflow nodes
-- while respecting dependencies.
--
-- >>> executeWorkflow myWorkflow "input"
-- Right 42
executeWorkflow :: Workflow a b -> a -> IO (Either WorkflowError b)
executeWorkflow workflow input = do
  -- Implementation here
```

### GHC Extensions

Use extensions judiciously. Common ones in this project:
- `GADTs`
- `RankNTypes`
- `OverloadedStrings`
- `ScopedTypeVariables`

Document any new extensions in the module header.

## Testing Guidelines

### Test Structure

- Unit tests for individual functions
- Property tests for laws and invariants
- Integration tests for workflows
- Performance benchmarks

### Writing Tests

```haskell
spec :: Spec
spec = do
  describe "Flow.Core" $ do
    it "satisfies arrow identity law" $ do
      property $ \x -> 
        runFlow (arr id) x == x
    
    it "composes workflows correctly" $ do
      let workflow = proc x -> do
            y <- step1 -< x
            z <- step2 -< y
            returnA -< z
      runFlow workflow testInput `shouldBe` expectedOutput
```

### Running Tests

```bash
# Run all tests
cabal test

# Run specific test suite
cabal test algoflow-clean:test:spec

# Run with coverage (when implemented)
cabal test --enable-coverage
```

## Documentation

### Code Documentation

- Use Haddock format for all public APIs
- Include usage examples
- Document assumptions and limitations
- Explain complex algorithms

### Project Documentation

- Update README.md for user-facing changes
- Update DESIGN.md for architectural changes
- Add examples to demonstrate new features
- Keep CLAUDE.md updated for AI assistance

## Submitting Changes

### Pull Request Process

1. **Update your fork**:
   ```bash
   git fetch upstream
   git checkout main
   git merge upstream/main
   ```

2. **Rebase your feature branch**:
   ```bash
   git checkout feature/your-feature
   git rebase main
   ```

3. **Push to your fork**:
   ```bash
   git push origin feature/your-feature
   ```

4. **Create Pull Request**:
   - Use the PR template
   - Reference related issues
   - Describe your changes clearly
   - Include your LESSONS.md entry

### PR Checklist

- [ ] Tests pass locally (`cabal test`)
- [ ] Code follows style guidelines
- [ ] Documentation updated
- [ ] LESSONS.md entry added
- [ ] PR description is complete
- [ ] Related issues are linked

### Branch Protection

The `main` branch has protection rules:
- Requires PR review approval
- CI must pass
- Must be up-to-date with main
- No force pushes allowed

## Learning Documentation

### LESSONS.md Updates

For each PR, add a "Stop and Jot" reflection to LESSONS.md:

```markdown
## YYYY-MM-DD - Brief Description
**Agent: Your Name**

### 3 Things I Learned
1. [First learning]
2. [Second learning]
3. [Third learning]

### 2 Highlights/Interesting Facts
1. [First highlight]
2. [Second highlight]

### 1 Question I Still Have
- [Your question]
```

This helps us track insights and improve the project collectively.

## Getting Help

- **Issues**: Check existing issues or create a new one
- **Discussions**: Use GitHub Discussions for questions
- **Documentation**: Read DESIGN.md for architectural context
- **Examples**: Study the examples in the codebase

## Recognition

Contributors will be recognized in:
- GitHub contributors page
- Release notes
- Project documentation

Thank you for contributing to AlgoFlow!