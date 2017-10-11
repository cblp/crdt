module CRDT.Cv.TwoPSet
    ( TwoPSet (..)
    , add
    , initial
    , lookup
    , remove
    ) where

import           Prelude hiding (lookup)

import           CRDT.Cv.GSet (GSet)
import qualified CRDT.Cv.GSet as GSet
import           Data.Semigroup (Semigroup ((<>)))
import           Data.Semilattice (Semilattice)

data TwoPSet a = TwoPSet
    { additions :: GSet a
    , removals :: GSet a
    }
    deriving (Eq, Ord, Show)

instance Ord a => Semigroup (TwoPSet a) where
    TwoPSet{additions = a1, removals = r1} <> TwoPSet a2 r2 =
        TwoPSet (a1 <> a2) (r1 <> r2)

instance Ord a => Semilattice (TwoPSet a)

add :: Ord a => a -> TwoPSet a -> TwoPSet a
add e (TwoPSet a r) = TwoPSet (GSet.add e a) r

initial :: TwoPSet a
initial = TwoPSet GSet.initial GSet.initial

lookup :: Ord a => a -> TwoPSet a -> Bool
lookup e (TwoPSet a r) = GSet.lookup e a && not (GSet.lookup e r)

remove :: Ord a => a -> TwoPSet a -> TwoPSet a
remove e tpset =
    if GSet.lookup e a then
        TwoPSet a (GSet.add e (removals tpset))
    else
        tpset
  where a = additions tpset
