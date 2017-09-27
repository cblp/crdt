module CRDT.GSet.Cv
    ( add
    , initial
    , query
    ) where

import qualified Data.Set as Set

import CRDT.GSet.Cv.Internal

-- | update
add :: Ord a => a -> GSet a -> GSet a
add e (GSet set) = GSet (Set.insert e set)

-- | initialization
initial :: GSet a
initial = GSet Set.empty

query :: Ord a => a -> GSet a -> Bool
query e (GSet set) = Set.member e set
