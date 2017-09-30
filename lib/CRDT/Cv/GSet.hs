{-# OPTIONS_GHC -Wno-orphans #-}

module CRDT.Cv.GSet
    ( GSet
    , add
    , initial
    , query
    ) where

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

query :: Ord a => a -> GSet a -> Bool
query = Set.member
