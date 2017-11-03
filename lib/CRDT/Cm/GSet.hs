{-# LANGUAGE TypeFamilies #-}

module CRDT.Cm.GSet
    ( GSet (..)
    ) where

import           Prelude hiding (lookup)

import           Data.Set (Set)
import qualified Data.Set as Set

import           CRDT.Cm (CausalOrd (..), CmRDT (..))

newtype GSet a = Add a
    deriving (Eq, Show)

instance Ord a => CmRDT (GSet a) where
    type Payload (GSet a) = Set a

    apply (Add a) = Set.insert a

instance Eq a => CausalOrd (GSet a) where
    affects _ _ = False
