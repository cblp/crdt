module CRDT.Cv.TPSet
    ( TPSet (..)
    , add
    , initial
    , query
    , remove
    ) where

import           Data.Semigroup (Semigroup ((<>)))
import           Data.Semilattice (Semilattice)
import           CRDT.Cv.GSet   (GSet)
import qualified CRDT.Cv.GSet as GSet

data TPSet a = TPSet (GSet a) (GSet a)
    deriving (Eq, Ord, Show)

instance Ord a => Semigroup (TPSet a) where
    TPSet a1 r1 <> TPSet a2 r2 = TPSet (a1 <> a2) (r1 <> r2)

instance Ord a => Semilattice (TPSet a)

add :: Ord a => a -> TPSet a -> TPSet a
add e (TPSet a r) = TPSet (GSet.add e a) r

initial :: TPSet a
initial = TPSet GSet.initial GSet.initial

query :: Ord a => a -> TPSet a -> Bool
query e (TPSet a r) = GSet.query e a && not ( GSet.query e r)

remove :: Ord a => a -> TPSet a -> TPSet a
remove e (TPSet a r) = TPSet a (GSet.add e r)
