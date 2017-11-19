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

instance Ord a => Semigroup (ORSet a) where
    ORSet s1 <> ORSet s2 = ORSet $ Map.unionWith (Map.unionWith (&&)) s1 s2

instance Ord a => Semilattice (ORSet a)

initial :: ORSet a
initial = ORSet Map.empty

add :: Ord a => Pid -> a -> ORSet a -> ORSet a
add pid e (ORSet s) = ORSet (Map.alter add1 e s)
  where
    add1 = Just . add2 . fromMaybe Map.empty
    add2 tags = Map.insert (pid, fromIntegral $ length tags) True tags

remove :: Ord a => a -> ORSet a -> ORSet a
remove e (ORSet s) = ORSet $ Map.adjust ($> False) e s

lookup :: Ord a => a -> ORSet a -> Bool
lookup e (ORSet s) = or . fromMaybe Map.empty $ Map.lookup e s
