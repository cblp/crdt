module CRDT.Cv.MV.Internal where

import           Debug.Trace

import           Algebra.Lattice.Ordered (Ordered (..))
import           Algebra.PartialOrd (PartialOrd (leq))
import           Data.Foldable (toList)
import           Data.Function (on)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.Semigroup (Semigroup ((<>)))
import           Data.Semilattice (Semilattice)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Numeric.Natural (Natural)

newtype Version = Version (IntMap Natural)
    deriving (Eq, Ord, Show)

instance PartialOrd Version where
    leq (Version v) (Version w) =
        trace ("v = " ++ show (IntMap.assocs v) ++ ", w = " ++ show (IntMap.assocs w)) $
        trace' "\tleq = " $
        (IntMap.isSubmapOfBy leq `on` IntMap.map Ordered) (IntMap.filter (/= 0) v) w

data Item a = Item{value :: a, version :: Version}
    deriving (Eq, Ord, Show)

-- | Multi-value register
newtype MV a = MV (Set (Item a))
    deriving (Eq)

-- | TODO(Syrovetsky, 2017-09-28) Just 1 `leq` Nothing??? check presence
instance Ord a => Semigroup (MV a) where
    MV a <> MV b =
        let a' = Set.filter (notDominatedBy (Set.map version b) . version) a
            b' = Set.filter (notDominatedBy (Set.map version a) . version) b
        in  MV $ Set.union a' b'

notDominatedBy :: Set Version -> Version -> Bool
notDominatedBy c v =
    and [ concurrent || ge
        | w <- toList c
        , let ge = w `leq` v
              le = v `leq` w
              concurrent = not ge && not le
        ]

instance Ord a => Semilattice (MV a)

trace' :: Show a => String -> a -> a
trace' msg a = trace (msg ++ show a) a
