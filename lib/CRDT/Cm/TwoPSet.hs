{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

-- | TODO(cblp, 2017-09-29) USet?
module CRDT.Cm.TwoPSet
    ( TwoPSet (..)
    , initial
    , query
    ) where

import           Control.Monad (guard)
import           Data.Set (Set)
import qualified Data.Set as Set

import           CRDT.Cm (CausalOrd (..), CmRDT (..))
import qualified CRDT.Cm as Cm

data TwoPSet a = Add a | Remove a
    deriving (Eq, Show)

instance Ord a => CmRDT (TwoPSet a) where
    type Payload (TwoPSet a) = Set a

    makeOp op s = case op of
        Add _     -> Just (pure op)
        Remove a  -> guard (Set.member a s) *> Just (pure op)

    apply = \case
        Add a     -> Set.insert a
        Remove a  -> Set.delete a

instance Eq a => CausalOrd (TwoPSet a) where
    Add b `precedes` Remove a = a == b -- `Remove e` can occur only after `Add e`
    _     `precedes` _        = False  -- Any other are not ordered

initial :: Set a
initial = Set.empty

query :: (Ord a, Foldable f) => f (TwoPSet a) -> Set a
query = Cm.query initial
