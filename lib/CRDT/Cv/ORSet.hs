module CRDT.Cv.ORSet
    ( ORSet (..)
    , add
    , initial
    , remove
    , lookup
    ) where

import           Prelude hiding (lookup)

import           Data.Functor (($>))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Semigroup (Semigroup, (<>))
import           Numeric.Natural (Natural)

import           CRDT.LamportClock (Pid)
import           Data.Semilattice (Semilattice)

type Tag = (Pid, Natural)

newtype ORSet a = ORSet (Map a (Map Tag Bool))
    deriving (Eq, Show)

overORSet :: (Map a (Map Tag Bool) -> Map b (Map Tag Bool)) -> ORSet a -> ORSet b
overORSet f (ORSet s) = ORSet (f s)

unpack :: ORSet a -> Map a (Map Tag Bool)
unpack (ORSet s) = s

instance Ord a => Semigroup (ORSet a) where
    ORSet s1 <> ORSet s2 = ORSet $ Map.unionWith (Map.unionWith (&&)) s1 s2

instance Ord a => Semilattice (ORSet a)

initial :: ORSet a
initial = ORSet Map.empty

add :: Ord a => Pid -> a -> ORSet a -> ORSet a
add pid = overORSet . Map.alter add1
  where
    add1 = Just . add2 . fromMaybe Map.empty
    add2 tags = Map.insert (pid, fromIntegral $ length tags) True tags

remove :: Ord a => a -> ORSet a -> ORSet a
remove = overORSet . Map.adjust ($> False)

lookup :: Ord a => a -> ORSet a -> Bool
lookup e = or . fromMaybe Map.empty . Map.lookup e . unpack
