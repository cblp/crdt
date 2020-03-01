module CRDT.Cv.TwoPSet
    ( TwoPSet (..)
    , add
    , initial
    , member
    , remove
    , singleton
    , isKnown
    ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)

import           Data.Semilattice (Semilattice)

newtype TwoPSet a = TwoPSet (Map a Bool)
    deriving (Eq, Show)

instance Ord a => Semigroup (TwoPSet a) where
    TwoPSet m1 <> TwoPSet m2 = TwoPSet (Map.unionWith (&&) m1 m2)

instance Ord a => Semilattice (TwoPSet a)

add :: Ord a => a -> TwoPSet a -> TwoPSet a
add e (TwoPSet m) = TwoPSet (Map.insertWith (&&) e True m)

initial :: TwoPSet a
initial = TwoPSet Map.empty

member :: Ord a => a -> TwoPSet a -> Bool
member e (TwoPSet m) = fromMaybe False $ Map.lookup e m

remove :: Ord a => a -> TwoPSet a -> TwoPSet a
remove e (TwoPSet m) = TwoPSet $ Map.adjust (const False) e m

singleton :: Ord a => a -> TwoPSet a
singleton a = add a initial

-- | XXX Internal
isKnown :: Ord a => a -> TwoPSet a -> Bool
isKnown e (TwoPSet m) = Map.member e m
