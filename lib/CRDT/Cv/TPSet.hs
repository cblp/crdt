module CRDT.Cv.TPSet
    ( TPSet (..)
    , add
    , initial
    , lookup
    , remove
    ) where

import           Prelude hiding (lookup)

import           Data.Semigroup (Semigroup ((<>)))
import           Data.Semilattice (Semilattice)
import           CRDT.Cv.GSet   (GSet)
import qualified CRDT.Cv.GSet as GSet

data TPSet a = TPSet{ additions :: GSet a
                    , removals :: GSet a
                    } deriving (Eq, Ord, Show)

instance Ord a => Semigroup (TPSet a) where
    TPSet a1 r1 <> TPSet a2 r2 = TPSet (a1 <> a2) (r1 <> r2)

instance Ord a => Semilattice (TPSet a)

add :: Ord a => a -> TPSet a -> TPSet a
add e (TPSet a r) = TPSet (GSet.add e a) r

initial :: TPSet a
initial = TPSet GSet.initial GSet.initial

lookup :: Ord a => a -> TPSet a -> Bool
lookup e (TPSet a r) = GSet.query e a && not (GSet.query e r)

remove :: Ord a => a -> TPSet a -> TPSet a
remove e tpset =
    if GSet.query e a then
        TPSet a (GSet.add e (removals tpset))
    else
        tpset
  where a = additions tpset
