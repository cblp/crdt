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

import           CRDT.LamportClock (Process)
import           CRDT.LWW (LWW)
import qualified CRDT.LWW as LWW
import           Data.Semilattice (Semilattice)

newtype LwwElementSet a = LES (Map a (LWW Bool))
    deriving (Eq, Show)

instance Ord a => Semigroup (LwwElementSet a) where
    LES m1 <> LES m2 = LES (Map.unionWith (<>) m1 m2)

instance Ord a => Semilattice (LwwElementSet a)

initial :: LwwElementSet a
initial = LES Map.empty

add :: Ord a => a -> LwwElementSet a -> Process (LwwElementSet a)
add value (LES m) = do
    tag <- LWW.initial True
    pure . LES $ Map.insertWith (<>) value tag m

remove :: Ord a => a -> LwwElementSet a -> Process (LwwElementSet a)
remove value (LES m) = do
    tag <- LWW.initial False
    pure . LES $ Map.insertWith (<>) value tag m

lookup :: Ord a => a -> LwwElementSet a -> Bool
lookup value (LES m) = Map.member value m
