{-# LANGUAGE TypeFamilies #-}

module CRDT.Cm.Counter
    ( Counter (..)
    , CounterOp (..)
    , initial
    ) where

import           Algebra.PartialOrd (PartialOrd (..))
import           CRDT.Cm (CmRDT (..), Update (..))

newtype Counter a = Counter a
    deriving (Show)

data CounterOp = Increment | Decrement
    deriving (Bounded, Enum, Eq, Show)

instance (Num a, Eq a) => CmRDT (Counter a) where
    type Op (Counter a) = CounterOp

    update op = Update
        { atSource = id
        , downstream = case op of
            Increment -> \(Counter c) -> Counter (c + 1)
            Decrement -> \(Counter c) -> Counter (c - 1)
        }

    type Query (Counter a) = a

    query (Counter c) = c

-- | Empty order, allowing arbitrary reordering
instance PartialOrd CounterOp where
    leq _ _ = False

initial :: Num a => Counter a
initial = Counter 0
