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

import           CRDT.HybridClock (HybridTime, Process, getTime)
import           Data.Semilattice (Semilattice)

-- TODO LES (Map a (LWW Bool))?
newtype LwwElementSet a = LES (Map a (HybridTime, Bool))
    deriving (Eq, Show)

instance Ord a => Semigroup (LwwElementSet a) where
    LES m1 <> LES m2 = LES (Map.unionWith lastWriteWins m1 m2)

instance Ord a => Semilattice (LwwElementSet a)

lastWriteWins :: Eq a => (HybridTime, a) -> (HybridTime, a) -> (HybridTime, a)
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

add :: Ord a => a -> LwwElementSet a -> Process (LwwElementSet a)
add e (LES m) = do
    t <- getTime
    pure . LES $ Map.insertWith lastWriteWins e (t, True) m

remove :: Ord a => a -> LwwElementSet a -> Process (LwwElementSet a)
remove e (LES m) = do
    t <- getTime
    pure . LES $ Map.insertWith lastWriteWins e (t, False) m

lookup :: Ord a => a -> LwwElementSet a -> Bool
lookup e (LES m) = Map.member e m
