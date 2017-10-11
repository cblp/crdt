{-# OPTIONS_GHC -Wno-orphans #-}

module CRDT.Cv.Max
    ( Max (..)
    , initial
    , query
    ) where

import           Data.Semigroup (Max (..))
import           Data.Semilattice (Semilattice)

instance Ord a => Semilattice (Max a)

-- | Construct new value
initial :: a -> Max a
initial = Max

query :: Max a -> a
query = getMax
