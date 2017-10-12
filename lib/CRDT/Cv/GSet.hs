module CRDT.Cv.GSet
    ( GSet
    , add
    , initial
    , lookup
    ) where

import           Prelude hiding (lookup)

import           Data.Set (Set)
import qualified Data.Set as Set

-- | Grow-only set
type GSet = Set

-- | update
add :: Ord a => a -> GSet a -> GSet a
add = Set.insert

-- | initialization
initial :: GSet a
initial = Set.empty

-- | lookup query
lookup :: Ord a => a -> GSet a -> Bool
lookup = Set.member
