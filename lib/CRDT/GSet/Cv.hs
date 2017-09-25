module CRDT.GSet.Cv where

import qualified Data.Set as Set

import CRDT.GSet.Cv.Internal

-- | update
add :: Ord a => GSet a -> a -> GSet a
add (GSet set) e = GSet (Set.insert e set)

-- | initialization
initial :: GSet a
initial = GSet (Set.empty)

query :: Ord a => GSet a -> a -> Bool
query (GSet set) e = Set.member e set
