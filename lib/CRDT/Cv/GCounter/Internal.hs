module CRDT.Cv.GCounter.Internal where

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.Semigroup (Semigroup ((<>)))

import           CRDT.Cv (CvRDT)

-- | Grow-only counter.
newtype GCounter a = GCounter (IntMap a)
    deriving (Eq, Show)

instance Ord a => Semigroup (GCounter a) where
    GCounter x <> GCounter y = GCounter $ IntMap.unionWith max x y

instance Ord a => CvRDT (GCounter a)
