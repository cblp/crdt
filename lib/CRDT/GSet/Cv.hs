module CRDT.GSet.Cv
    ( add
    , initial
    , query
    ) where

import qualified Data.Set as Set

import CRDT.GSet.Cv.Internal

-- | update
add :: Ord a => a -> GSet a -> GSet a
add = Set.insert

-- | initialization
initial :: GSet a
initial = Set.empty

query :: Ord a => a -> GSet a -> Bool
query = Set.member
