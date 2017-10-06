{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module CRDT.Cm.GSet
    ( GSet (..)
    ) where

import           Prelude hiding (lookup)

import           Algebra.PartialOrd (PartialOrd (leq))
import           Data.Set (Set)
import qualified Data.Set as Set

import           CRDT.Cm (CmRDT (..))

newtype GSet a = Add a
    deriving (Eq, Show)

instance Ord a => CmRDT (GSet a) where
    type Payload  (GSet a) = Set a

    updateDownstream (Add a) = Set.insert a

instance Eq a => PartialOrd (GSet a) where
    leq _ _ = False
