module CRDT.GCounter.Cv.Internal where

import           Data.Semigroup (Semigroup ((<>)))
import           Data.Vector    (Vector)
import qualified Data.Vector    as Vector

import CRDT.Cv (CvRDT)

-- | Grow-only counter.
newtype GCounter a = GCounter (Vector a)
    deriving (Eq, Show)

instance Ord a => Semigroup (GCounter a) where
    GCounter x <> GCounter y = GCounter $ Vector.zipWith max x y

instance Ord a => CvRDT (GCounter a)
