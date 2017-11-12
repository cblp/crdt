module CRDT.Cv.ORSet
    ( ORSet (..)
    , add
    , initial
    , remove
    , lookup
    ) where

import           Prelude hiding (lookup)

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Semigroup (Semigroup ((<>)))
import           Data.Semilattice (Semilattice)
import           Data.Set (Set)
import qualified Data.Set as Set

import           CRDT.LamportClock (Pid)

newtype ORSet a = ORSet (Map Pid (Set a))
    deriving (Eq, Ord, Show)

instance Ord a => Semigroup (ORSet a) where
    ORSet map1 <> ORSet map2 =
        ORSet (Map.unionWith Set.union map1 map2)

instance Ord a => Semilattice (ORSet a)

initial :: ORSet a
initial = ORSet Map.empty

add :: Ord a => Pid -> a -> ORSet a -> ORSet a
add pid e (ORSet storage) =
    ORSet (Map.insertWith Set.union pid (Set.singleton e) storage)

remove :: Ord a => Pid -> a -> ORSet a -> ORSet a
remove pid e (ORSet storage) =
    ORSet (Map.insertWith (Set.\\) pid (Set.singleton e) storage)

lookup :: Ord a => a -> ORSet a -> Bool
lookup e (ORSet storage) =
    Map.foldr (\a b -> b || Set.member e a) False storage
