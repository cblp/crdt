{-# OPTIONS_GHC -Wno-orphans #-}

module CRDT.Cv.Max
    ( Max
    , point
    , query
    ) where

import           Data.Semigroup (Max (..))
import           Data.Semilattice (Semilattice)

instance Ord a => Semilattice (Max a)

-- | Construct new value
point :: a -> Max a
point = Max

query :: Max a -> a
query = getMax
