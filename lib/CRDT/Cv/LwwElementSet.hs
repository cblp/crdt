{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module CRDT.Cv.LwwElementSet
    ( LwwElementSet (..)
    , add
    , initial
    , lookup
    , remove
    ) where

import           Prelude hiding (lookup)

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Semigroup (Semigroup (..))
import           Data.Semilattice (Semilattice)

import           GlobalTime (Time)

newtype LwwElementSet a = LES (Map a (Time, Bool))
    deriving (Eq, Show)

instance Ord a => Semigroup (LwwElementSet a) where
    LES m1 <> LES m2 = LES (Map.unionWith lastWriteWins m1 m2)

instance Ord a => Semilattice (LwwElementSet a)

lastWriteWins :: Eq a => (Time, a) -> (Time, a) -> (Time, a)
lastWriteWins e1@(t1, a1) e2@(t2, a2) =
    case compare t1 t2 of
        LT -> e2
        GT -> e1
        EQ ->
            if a1 == a2 then
                e1
            else
                error "LwwElementSet assumes timestamps to be unique"

initial :: LwwElementSet a
initial = LES Map.empty

add :: Ord a => a -> Time -> LwwElementSet a -> LwwElementSet a
add e t (LES m) = LES (Map.insertWith lastWriteWins e (t, True) m)

remove :: Ord a => a -> Time -> LwwElementSet a -> LwwElementSet a
remove e t (LES m) = LES (Map.insertWith lastWriteWins e (t, False) m)

lookup :: Ord a => a -> LwwElementSet a -> Bool
lookup e (LES m) = Map.member e m
