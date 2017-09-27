module CRDT.Cv.GSet
    ( GSet
    , add
    , initial
    , query
    ) where

import qualified Data.Set as Set

import           CRDT.Cv.GSet.Internal

-- | update
add :: Ord a => a -> GSet a -> GSet a
add = Set.insert

-- | initialization
initial :: GSet a
initial = Set.empty

query :: Ord a => a -> GSet a -> Bool
query = Set.member
