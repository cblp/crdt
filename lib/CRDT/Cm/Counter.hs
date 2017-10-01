{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module CRDT.Cm.Counter
    ( Counter (..)
    , CounterOp (..)
    , initial
    ) where

import           Algebra.PartialOrd (PartialOrd (..))
import           CRDT.Cm (CmRDT (..))

newtype Counter a = Counter a
    deriving (Show)

data CounterOp a = Increment | Decrement
    deriving (Bounded, Enum, Eq, Show)

instance (Num a, Eq a) => CmRDT (Counter a) (CounterOp a) (CounterOp a) a where
    updateAtSource = pure
    updateDownstream = \case
        Increment -> \(Counter c) -> Counter (c + 1)
        Decrement -> \(Counter c) -> Counter (c - 1)
    view (Counter c) = c

-- | Empty order, allowing arbitrary reordering
instance PartialOrd (CounterOp a) where
    leq _ _ = False

initial :: Num a => Counter a
initial = Counter 0
