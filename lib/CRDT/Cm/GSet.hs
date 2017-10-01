{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module CRDT.Cm.GSet
    ( GSet
    , Add (..)
    , initial
    , lookup
    ) where

import           Prelude hiding (lookup)

import           Algebra.PartialOrd (PartialOrd (leq))
import           Data.Set (Set)
import qualified Data.Set as Set

import           CRDT.Cm (CmRDT (..))
import           CRDT.Cv.GSet (GSet)

initial :: GSet a
initial = Set.empty

-- | query lookup
lookup :: Ord a => a -> GSet a -> Bool
lookup = Set.member

newtype Add a = Add a
    deriving (Eq, Show)

instance Ord a => CmRDT (Set a) (Add a) (Add a) (Set a) where
    updateAtSource = pure
    updateDownstream (Add a) = Set.insert a
    view = id

instance Eq a => PartialOrd (Add a) where
    leq _ _ = False
