# Graph Runner - What Are We Actually Building?

## Core Problem

Run computational graphs where:
1. Nodes are computations (a → b)
2. Edges are data dependencies
3. Graphs can contain other graphs (composition)
4. No dependency cycles (DAG)
5. Parallel execution when possible

## The Simplest Thing That Works

If we strip away all the enterprise nonsense, we need:

1. **A computation**: `a → m b`
2. **Composition**: Ways to combine computations
3. **Execution**: Run the graph respecting dependencies

## Key Insights

1. **Graphs of graphs** = Just function composition with extra steps
2. **FSMs** = Special case where computations are state transitions
3. **Category theory** = Our computations should compose associatively with identity

That's it. Everything else is accidental complexity.
