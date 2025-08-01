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

## 2025-08-01 - Refactoring for Clarity: v2 to Cookbook Rename
**Agent: Claude Code**

### 3 Things I Learned
1. **Naming matters**: The "v2" directory name was confusing - it wasn't a version 2, but rather a simplified teaching implementation. "Cookbook" immediately communicates its purpose as a collection of examples and recipes.
2. **Decomposition is key**: When the user said "we need to rationally decompose our efforts," it highlighted the importance of breaking work into clear, focused changes rather than mixing concerns (like combining the rename with the data pipeline work).
3. **Git stash is your friend**: Using `git stash` to temporarily save work-in-progress allowed us to cleanly switch contexts and create focused PRs without losing progress.

### 2 Highlights/Interesting Facts
1. The cookbook implementation is only ~150 lines but demonstrates all core concepts (computational graphs, parallel execution, FSMs, graphs of graphs) that the main implementation covers in 800+ lines.
2. The project cleverly maintains two implementations with different goals: the main src/ for production-ready features and the cookbook/ for teaching and demonstrating concepts simply.

### 1 Question I Still Have
- Should the cookbook directory have its own README at the top level to make it more discoverable, or is the existing structure with README inside the directory sufficient?

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