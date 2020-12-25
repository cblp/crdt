{-# LANGUAGE FlexibleInstances #-}

module CRDT.Cv.LwwElementSet
    ( LwwElementSet (..)
    , add
    , initial
    , lookup
    , remove
    ) where

import           Prelude hiding (lookup)

import           Data.Foldable (for_)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           CRDT.LamportClock (Clock)
import           CRDT.LWW (LWW, advanceFromLWW)
import qualified CRDT.LWW as LWW
import           Data.Semilattice (Semilattice)

newtype LwwElementSet a = LES (Map a (LWW Bool))
    deriving (Eq, Show)

instance Ord a => Semigroup (LwwElementSet a) where
    LES m1 <> LES m2 = LES (Map.unionWith (<>) m1 m2)

instance Ord a => Semilattice (LwwElementSet a)

initial :: LwwElementSet a
initial = LES Map.empty

add :: (Ord a, Clock m) => a -> LwwElementSet a -> m (LwwElementSet a)
add value old@(LES m) = do
    advanceFromLES old
    tag <- LWW.initialize True
    pure . LES $ Map.insert value tag m

remove :: (Ord a, Clock m) => a -> LwwElementSet a -> m (LwwElementSet a)
remove value old@(LES m) = do
    advanceFromLES old
    tag <- LWW.initialize False
    pure . LES $ Map.insert value tag m

lookup :: Ord a => a -> LwwElementSet a -> Bool
lookup value (LES m) = maybe False LWW.query $ Map.lookup value m

advanceFromLES :: Clock m => LwwElementSet a -> m ()
advanceFromLES (LES m) = for_ m advanceFromLWW
