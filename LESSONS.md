# LESSONS.md

This file contains periodic "Stop and Jot" reflections from development sessions. Each entry follows the 3-2-1 format:
- 3 things I learned
- 2 highlights/interesting facts
- 1 question I still have

These reflections should be added at least once per pull request to capture learning and insights.

---

## 2025-08-01 - Initial Project Setup
**Agent: Claude Code**

### 3 Things I Learned
1. **Dual Implementation Strategy**: AlgoFlow has two implementations - a comprehensive one with multiple DSL approaches (Arrow, Free Monad, Graph, Type-level) and a simplified ~150-line version that demonstrates core concepts clearly.
2. **Type Safety as Documentation**: The project uses Haskell's type system not just for safety but as living documentation - types like GADTs and type families make the API self-documenting and prevent misuse at compile time.
3. **Missing Test Infrastructure**: While the project has test frameworks configured (Hspec + QuickCheck), no actual tests are implemented yet, which is unusual for a project emphasizing correctness through types.

### 2 Highlights/Interesting Facts
1. **Mathematical Foundation**: The project properly implements Category and Arrow laws with plans for formal verification - this is rare in workflow engines and shows commitment to correctness.
2. **Pedagogical Design**: The codebase explicitly serves as both a practical tool and a teaching example, with extensive documentation explaining not just "what" but "why" decisions were made.

### 1 Question I Still Have
- How does the performance of the type-safe approach compare to more dynamic workflow engines, especially for large computational graphs with thousands of nodes?

---

## 2025-08-01 - Repository Infrastructure Setup
**Agent: Claude Code**

### 3 Things I Learned
1. **GitHub Project Management Integration**: Setting up proper issue templates, PR templates, and branch protection creates a structured development workflow that guides contributors and maintains quality standards automatically.
2. **CI/CD Matrix Strategy**: Using a build matrix across multiple GHC versions (9.4, 9.6, 9.8) and operating systems (Ubuntu, macOS, Windows) ensures broad compatibility but requires careful exclusion rules to manage build time.
3. **Documentation as Code**: GitHub Actions can automatically build and deploy Haddock documentation to GitHub Pages, creating a self-updating documentation site that stays in sync with the code.

### 2 Highlights/Interesting Facts
1. **LESSONS.md as Team Learning Tool**: The stop-and-jot format (3-2-1) creates a low-friction way to capture insights and creates a historical record of project understanding that benefits future contributors.
2. **Issue Hierarchies**: Creating umbrella issues with sub-task checkboxes provides better project organization than flat issue lists, making it easier to track complex features like test suite implementation.

### 1 Question I Still Have
- How can we automate the GitHub Project board creation and configuration since the gh CLI requires interactive authentication for project scopes?

---

## 2025-08-03 - Core Type System Resolution & Compilation Success
**Agent: Claude Code**

### 3 Things I Learned
1. **TypeApplications Scoping Requirements**: TypeApplications syntax (`@Type`) requires explicit `forall` quantifiers in function signatures to work properly. Without `forall a b.`, the type variables are not in scope for the `@a` syntax, leading to "Not in scope" errors that are easy to misinterpret.
2. **MonadCatch vs Control.Exception**: When building polymorphic error handling, `catch` from `Control.Exception` only works in `IO`, but `catch` from `Control.Monad.Catch` works across any `MonadCatch` instance. This distinction is crucial for building generic workflow error handling.
3. **Build Environment Cascading Failures**: A corrupted cabal package cache can cause mysterious "package list does not exist" errors that persist across updates. The solution required completely clearing `~/.cabal/packages` and forcing a fresh download, not just running `cabal update`.

### 2 Highlights/Interesting Facts
1. **Multiple DSL Coexistence**: Successfully implemented three distinct workflow DSLs (Arrow-based, Free monad, Graph-based) that all compile and can interoperate. Each serves different use cases - Arrow for composition, Free for introspection, Graph for dependency analysis.
2. **Type-Driven Development Victory**: The extensive use of GADTs, type families, and phantom types initially created compilation challenges, but ultimately prevented numerous runtime errors and made the API self-documenting. The type system became a powerful ally once the constraints were satisfied.

### 1 Question I Still Have
- How will the performance characteristics differ between the Arrow-based workflows (using profunctors and composition) versus the Free monad approach (building ASTs then interpreting) when dealing with large, complex workflow graphs with hundreds of steps?

---

## 2025-08-03 - Sprint 1 Complete: All Tests Passing! 🎉
**Agent: Claude Code**

### 3 Things I Learned
1. **Haskell's type system is a powerful ally, not an enemy**: Initial GADT and RankNTypes errors seemed insurmountable, but once understood, they prevent entire categories of bugs. Type-driven development actually guides you to correct implementations - the compiler becomes your pair programmer.
2. **Atomic operations are crucial for concurrent state**: The cache hit count bug was a classic read-modify-write race condition. Using `modifyMVar` instead of separate `readMVar`/`modifyMVar_` ensures atomicity. This pattern appears everywhere in production Haskell code.
3. **Pure exceptions need forcing in IO context**: Division by zero (`100 \`div\` 0`) doesn't throw until the thunk is evaluated. Using `evaluate` forces evaluation within exception handlers, critical for catching arithmetic errors in workflow steps.

### 2 Highlights/Interesting Facts
1. **Multiple DSL approaches serve genuinely different needs**: Arrow-based workflows excel at point-free composition and parallel operations, Free monads provide introspection and interpretation flexibility, while Graph-based representations enable dependency analysis and visualization. Having all three isn't redundancy - it's flexibility.
2. **Property-based testing validates mathematical laws**: QuickCheck tests for Category/Arrow law compliance caught subtle violations we would have missed with example-based tests. When your tests are mathematical proofs, you gain extreme confidence in correctness.

### 1 Question I Still Have
- How can we make Haskell's type errors more beginner-friendly? Current errors like "Couldn't match type 'Workflow m0 Int Int' with 'Int -> m Int'" are cryptic. Could we use GHC's custom type errors or typed Template Haskell to provide better guidance?

**Sprint 1 Retrospective Note**: I'm glad we stuck with Haskell! The initial type system struggles were worth it for the confidence we now have in our implementation. Every test that passes validates not just behavior but mathematical laws.

---

<!-- Template for future entries:
## YYYY-MM-DD - Brief Description
**Agent: Claude Code**

### 3 Things I Learned
1. 
2. 
3. 

### 2 Highlights/Interesting Facts
1. 
2. 

### 1 Question I Still Have
- 
-->