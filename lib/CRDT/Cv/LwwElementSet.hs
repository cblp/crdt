{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module CRDT.Cv.LwwElementSet
    ( LwwElementSet(..)
    , add
    , initial
    , lookup
    , remove
    ) where

import           Prelude hiding (lookup)

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Semigroup (Semigroup ((<>)))
import           Data.Semilattice (Semilattice)

import LamportClock (Timestamp)

newtype LwwElementSet a = LES (Map a (Timestamp, Bool))

instance Ord a => Semigroup (LwwElementSet a) where
    LES m1 <> LES m2 =
        LES (Map.unionWith lastWriteWins m1 m2)

instance Ord a => Semilattice (LwwElementSet a)

lastWriteWins :: (Timestamp, Bool)
              -> (Timestamp, Bool)
              -> (Timestamp, Bool)
lastWriteWins e1 e2 =
    if fst e1 > fst e2 then
        e1
    else
        e2

initial :: LwwElementSet a
initial = LES Map.empty

add :: Ord a => a
    -> Timestamp
    -> LwwElementSet a
    -> LwwElementSet a
add e t (LES m) = LES (Map.insertWith lastWriteWins e (t, True) m)

remove :: Ord a => a
       -> Timestamp
       -> LwwElementSet a
       -> LwwElementSet a
remove e t (LES m) = LES (Map.insertWith lastWriteWins e (t, False) m)

lookup :: Ord a => a -> LwwElementSet a -> Bool
lookup e (LES m) = Map.member e m
