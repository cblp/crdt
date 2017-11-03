{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

-- | TODO(cblp, 2017-09-29) USet?
module CRDT.Cm.TwoPSet
    ( TwoPSet (..)
    ) where

import           Data.Set (Set)
import qualified Data.Set as Set

import           CRDT.Cm (CausalOrd (..), CmRDT (..))

data TwoPSet a = Add a | Remove a
    deriving (Eq, Show)

instance Ord a => CmRDT (TwoPSet a) where
    type Payload (TwoPSet a) = Set a

    precondition = \case
        Add _     -> const True
        Remove a  -> Set.member a

    apply = \case
        Add a     -> Set.insert a
        Remove a  -> Set.delete a

instance Eq a => CausalOrd (TwoPSet a) where
    Remove a `affects` Add b = a == b -- `Remove e` can occur only after `Add e`
    _        `affects` _     = False  -- Any other are not ordered
