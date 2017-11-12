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
import           Numeric.Natural(Natural)

import           CRDT.LamportClock (Pid)

type Tag = Natural

newtype ORSet a = ORSet (Map Pid (Map a (Tag, Bool)))
    deriving (Eq, Show)

instance Ord a => Semigroup (ORSet a) where
    ORSet map1 <> ORSet map2 =
        ORSet (Map.unionWith (Map.unionWith lww) map1 map2)
      where
        lww a b =
            let (f1, s1) = a
                (f2, s2) = b in
            case compare f1 f2 of
                EQ -> (f1, s1 && s2)
                LT -> a
                GT -> b

instance Ord a => Semilattice (ORSet a)

initial :: ORSet a
initial = ORSet Map.empty

add :: Ord a => Pid -> a -> ORSet a -> ORSet a
add pid e (ORSet storage) =
    ORSet (Map.alter add1 pid storage)
  where
    lww a =
        if snd a then
            a
        else
            (fst a + 1, True)
    add1 x = Just (maybe (Map.singleton e (0, True)) (Map.alter add2 e) x)
    add2 x = Just (maybe (0, True) lww x)

remove :: Ord a => Pid -> a -> ORSet a -> ORSet a
remove pid e (ORSet storage) =
    ORSet (Map.adjust (Map.adjust lww e) pid storage)
  where
    lww a =
        if snd a then
            (fst a, False)
        else
            a

lookup :: Ord a => a -> ORSet a -> Bool
lookup e (ORSet storage) =
    Map.foldr (\a b -> maybe False snd Map.lookup e a || b) False storage
