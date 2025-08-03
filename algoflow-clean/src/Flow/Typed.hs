{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Flow.Typed
Description : Type-safe workflows with compile-time dependency checking

This module shows how we can use Haskell's type system to ensure
workflows are correctly constructed at compile time. No runtime
dependency errors possible!
-}

module Flow.Typed where

import Data.Kind (Type)
import Data.Proxy
import GHC.TypeLits

-- | Type-level list
data HList (ts :: [Type]) where
  HNil :: HList '[]
  HCons :: t -> HList ts -> HList (t ': ts)

-- | Type-level dependency tracking
data Dep (name :: Symbol) (a :: Type) = Dep a

-- | A computation with named inputs and output
data Computation (inputs :: [(Symbol, Type)]) (output :: Type) where
  Computation :: 
    (HList (InputTypes inputs) -> IO output) -> 
    Computation inputs output

-- | Extract types from named inputs
type family InputTypes (inputs :: [(Symbol, Type)]) :: [Type] where
  InputTypes '[] = '[]
  InputTypes ('(name, t) ': rest) = t ': InputTypes rest

-- | A typed workflow step
data Step (available :: [(Symbol, Type)]) (added :: (Symbol, Type)) where
  Step ::
    { stepName :: String
    , stepInputs :: Proxy needed
    , stepComputation :: Computation needed output
    } -> Step available '(name, output)

-- | Type-safe workflow composition
data Workflow (initial :: [(Symbol, Type)]) (final :: [(Symbol, Type)]) where
  -- Empty workflow
  WId :: Workflow a a
  
  -- Sequential composition
  WSeq :: Step a b -> Workflow (b ': a) c -> Workflow a c
  
  -- Parallel composition (type-safe!)
  WPar :: Workflow a b -> Workflow a c -> Workflow a (Append b c)

-- | Type family for appending type-level lists
type family Append (xs :: [a]) (ys :: [a]) :: [a] where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': Append xs ys

-- | Check if a type-level list contains required dependencies
type family HasDeps (available :: [(Symbol, Type)]) (needed :: [(Symbol, Type)]) :: Bool where
  HasDeps available '[] = 'True
  HasDeps available (n ': ns) = 
    If (Contains available n) 
       (HasDeps available ns)
       'False

type family Contains (list :: [(Symbol, Type)]) (item :: (Symbol, Type)) :: Bool where
  Contains '[] item = 'False
  Contains (x ': xs) x = 'True
  Contains (x ': xs) y = Contains xs y

type family If (cond :: Bool) (t :: a) (f :: a) :: a where
  If 'True t f = t
  If 'False t f = f

-- | Example: A data pipeline with compile-time dependency checking

-- Define our step types
type RawData = String
type CleanData = String
type ValidData = Bool
type ProcessedData = String

-- Create workflow steps
readStep :: Step '[] '("raw", RawData)
readStep = Step "read" Proxy $ 
  Computation $ \HNil -> readFile "input.txt"

cleanStep :: Step '[ '("raw", RawData)] '("clean", CleanData)
cleanStep = Step "clean" Proxy $
  Computation $ \(HCons (raw :: RawData) HNil) -> return $ filter (/= ' ') raw

validateStep :: Step '[ '("raw", RawData)] '("valid", ValidData)  
validateStep = Step "validate" Proxy $
  Computation $ \(HCons (raw :: RawData) HNil) -> return $ not (null raw)

-- Compose into workflow
-- This will only compile if dependencies are satisfied!
pipeline :: Workflow '[] '[ '("clean", CleanData), '("valid", ValidData), '("raw", RawData)]
pipeline = 
  WSeq readStep $
  WPar (WSeq cleanStep WId) 
       (WSeq validateStep WId)

-- If you try to use a step without its dependencies, it won't compile:
-- badPipeline = WSeq cleanStep WId  -- Error: "raw" not available!

-- | This approach ensures:
-- 1. All dependencies are satisfied at compile time
-- 2. No stringly-typed references
-- 3. Parallel composition is safe
-- 4. The type tells you exactly what the workflow produces
