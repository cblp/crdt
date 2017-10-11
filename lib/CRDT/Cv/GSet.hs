{-# OPTIONS_GHC -Wno-orphans #-}

module CRDT.Cv.GSet
    ( GSet
    , add
    , initial
    , lookup
    ) where

import           Prelude hiding (lookup)

import           Data.Semilattice (Semilattice)
import           Data.Set (Set)
import qualified Data.Set as Set

-- | Grow-only set
type GSet = Set

instance Ord a => Semilattice (Set a)

-- | update
add :: Ord a => a -> GSet a -> GSet a
add = Set.insert

-- | initialization
initial :: GSet a
initial = Set.empty

-- | lookup query
lookup :: Ord a => a -> GSet a -> Bool
lookup = Set.member
