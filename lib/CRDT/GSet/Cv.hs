module CRDT.GSet.Cv where

import qualified Data.Set as Set

import CRDT.GSet.Cv.Iternal

-- | update
add :: Ord a => GSet a -> a -> GSet a
add gset e = Set.insert e gset

-- | initialization
initial :: GSet a
initial = Set.empty

query :: Ord a => GSet a -> a -> Bool
query gset e = Set.member e gset
