{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module CRDT.Cm.Counter
    ( Counter (..)
    , CounterOp (..)
    , initial
    ) where

import           Algebra.PartialOrd (PartialOrd (..))
import           Data.Observe (Observe (..))

import           CRDT.Cm (CmRDT (..))

newtype Counter a = Counter a
    deriving (Show)

data CounterOp a = Increment | Decrement
    deriving (Bounded, Enum, Eq, Show)

instance (Num a, Eq a) => CmRDT (Counter a) (CounterOp a) (CounterOp a) where
    updateAtSource = pure
    updateDownstream = opToFunc

instance Observe (Counter a) where
    type Observed (Counter a) = a
    observe (Counter c) = c

-- | Empty order, allowing arbitrary reordering
instance PartialOrd (CounterOp a) where
    leq _ _ = False

initial :: Num a => Counter a
initial = Counter 0

opToFunc :: Num a => CounterOp a -> Counter a -> Counter a
opToFunc op (Counter c) =
    Counter $ case op of
        Increment -> c + 1
        Decrement -> c - 1
