module CRDT.GCounter.Cv.Internal where

import           Data.Semigroup (Semigroup ((<>)))
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import CRDT.Cv (CvRDT)

-- | Grow-only counter.
newtype GCounter a = GCounter (IntMap a)
    deriving (Eq, Show)

instance Ord a => Semigroup (GCounter a) where
    GCounter x <> GCounter y = GCounter $ IntMap.unionWith max x y

instance Ord a => CvRDT (GCounter a)
