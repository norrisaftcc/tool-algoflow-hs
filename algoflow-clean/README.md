# AlgoFlow Clean

A pure functional workflow engine that actually leverages Haskell's type system.

## Core Concept

We're building a system to:
1. Define computational steps with explicit dependencies
2. Automatically resolve execution order
3. Run steps in parallel when possible
4. Handle errors gracefully
5. Cache results when sensible

## The Haskell Way

Instead of stringly-typed everything, we'll use:
- Proper algebraic data types
- Type-safe dependency tracking
- Free monads or similar for the DSL
- Profunctors/Categories for composition

## What Makes This Different

The original code tried to be "enterprise-y" with ByteStrings everywhere and Text keys.
We'll instead:
- Use the type system to prevent errors at compile time
- Make illegal states unrepresentable
- Leverage category theory where it actually helps
- Keep it simple and composable
