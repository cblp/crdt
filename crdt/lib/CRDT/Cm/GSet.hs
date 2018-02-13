{-# LANGUAGE TypeFamilies #-}

module CRDT.Cm.GSet
    ( GSet (..)
    ) where

import           Data.Set (Set)
import qualified Data.Set as Set

import           CRDT.Cm (CausalOrd (..), CmRDT (..))

newtype GSet a = Add a
    deriving (Eq, Show)

instance Ord a => CmRDT (GSet a) where
    type Payload (GSet a) = Set a

    initial = Set.empty

    apply (Add a) = Set.insert a

instance CausalOrd (GSet a) where
    precedes _ _ = False
