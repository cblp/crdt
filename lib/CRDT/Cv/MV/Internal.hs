module CRDT.Cv.MV.Internal where

import           Algebra.Lattice.Ordered (Ordered)
import           Algebra.PartialOrd (PartialOrd (leq))
import           Data.Foldable (toList)
import           Data.IntMap (IntMap)
import           Data.Semigroup (Semigroup ((<>)))
import           Data.Semilattice (Semilattice)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Numeric.Natural (Natural)

type Version = IntMap (Ordered Natural)

-- | Multi-value register
newtype MV a = MV (Set (a, Version))
    deriving (Eq, Show)

-- | TODO(Syrovetsky, 2017-09-28) Just 1 `leq` Nothing??? check presence
instance Ord a => Semigroup (MV a) where
    MV a <> MV b =
        let a' = Set.filter (not . dominatedBy b) a
            b' = Set.filter (not . dominatedBy a) b
        in  MV $ Set.union a' b'
      where
        dominatedBy c (_, v) = or [leq v w | (_, w) <- toList c]

instance Ord a => Semilattice (MV a)
