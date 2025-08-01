{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Flow.Core
Description : Core types for the workflow engine

This module defines the core abstractions for building computational workflows.
Instead of stringly-typed steps and ByteString everywhere, we use proper
algebraic data types and leverage Haskell's category theory abstractions.
-}

module Flow.Core 
  ( -- * Core Types
    Flow(..)
  , NamedFlow(..)
  , Workflow(..)
    
    -- * Smart Constructors
  , step
  , (>>>)
  , (***)
  , cache
  , recover
    
    -- * Interpreters
  , interpret
  ) where

import Control.Category
import Control.Arrow
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO)
import Data.Profunctor
import Data.Text (Text)
import Data.Typeable (Typeable)
import Prelude hiding ((.), id)

-- | A computation that takes input of type 'a' and produces output of type 'b'.
-- This is essentially a Kleisli arrow, which forms a category.
newtype Flow m a b = Flow { runFlow :: a -> m b }

-- | Category instance - this gives us composition for free
instance Monad m => Category (Flow m) where
  id = Flow return
  (Flow g) . (Flow f) = Flow (f >=> g)

-- | Arrow instance for parallel composition
instance Monad m => Arrow (Flow m) where
  arr f = Flow (return . f)
  first (Flow f) = Flow $ \(a, c) -> do
    b <- f a
    return (b, c)

-- | Profunctor instance for input/output mapping
instance Functor m => Profunctor (Flow m) where
  dimap f g (Flow h) = Flow (fmap g . h . f)

-- | Strong profunctor for working with products
instance Functor m => Strong (Flow m) where
  first' (Flow f) = Flow $ \(a, c) -> fmap (, c) (f a)

-- | Named flows for debugging and caching
data NamedFlow m a b = NamedFlow
  { flowName :: Text
  , flowComputation :: Flow m a b
  } deriving (Typeable)

-- | A workflow is a graph of computations.
-- We use a GADT to ensure type safety and make illegal states unrepresentable.
data Workflow m a b where
  -- | Identity workflow (does nothing)
  Id :: Workflow m a a
  
  -- | Sequential composition: run first workflow, then second
  Seq :: Workflow m a b -> Workflow m b c -> Workflow m a c
  
  -- | Parallel composition: run both workflows on paired input
  Par :: Workflow m a b -> Workflow m c d -> Workflow m (a, c) (b, d)
  
  -- | A named computational step
  Step :: NamedFlow m a b -> Workflow m a b
  
  -- | Cache the results of a workflow
  Cache :: Text -> Workflow m a b -> Workflow m a b
  
  -- | Error recovery: if first workflow fails, try recovery
  Recover :: Workflow m a b -> Workflow m a b -> Workflow m a b

-- | Interpret a workflow into an actual computation.
-- This is where the abstract syntax tree becomes executable.
interpret :: MonadIO m => Workflow m a b -> Flow m a b
interpret Id = id
interpret (Seq w1 w2) = interpret w2 . interpret w1
interpret (Par w1 w2) = interpret w1 *** interpret w2
interpret (Step (NamedFlow _ f)) = f
interpret (Cache key w) = Flow $ \a -> do
  -- TODO: Implement actual caching logic
  -- For now, just run the workflow
  runFlow (interpret w) a
interpret (Recover w recovery) = Flow $ \a -> do
  -- TODO: Implement actual error handling
  -- For now, just run the main workflow
  runFlow (interpret w) a

-- | Smart constructor for creating a named step
step :: Text -> (a -> m b) -> Workflow m a b
step name f = Step (NamedFlow name (Flow f))

-- | Sequential composition operator
(>>>) :: Workflow m a b -> Workflow m b c -> Workflow m a c
(>>>) = Seq
infixr 1 >>>

-- | Parallel composition operator
(***) :: Workflow m a b -> Workflow m c d -> Workflow m (a, c) (b, d)
(***) = Par
infixr 3 ***

-- | Cache a workflow's results
cache :: Text -> Workflow m a b -> Workflow m a b
cache = Cache

-- | Add error recovery to a workflow
recover :: Workflow m a b -> Workflow m a b -> Workflow m a b
recover = Recover
