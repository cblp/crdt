{-# LANGUAGE TypeFamilies #-}

-- | TODO(cblp, 2017-09-29) USet?
module CRDT.Cm.TwoPSet
    ( TwoPSet (..)
    ) where

import           Control.Monad (guard)
import           Data.Set (Set)
import qualified Data.Set as Set

import           CRDT.Cm (CausalOrd (..), CmRDT (..))

data TwoPSet a = Add a | Remove a
    deriving (Eq, Show)

instance Ord a => CmRDT (TwoPSet a) where
    type Payload (TwoPSet a) = Set a

    makeOp op s = case op of
        Add _     -> Just (pure op)
        Remove a  -> guard (Set.member a s) *> Just (pure op)

    apply op s = case op of
        Add a     -> Just $ Set.insert a s
        Remove a  ->
            -- Just . Set.delete a -- TODO check precondition?
            if a `Set.member` s then
                Just $ Set.delete a s
            else
                Nothing

instance Eq a => CausalOrd (TwoPSet a) where
    Add b `precedes` Remove a = a == b -- `Remove e` can occur only after `Add e`
    _     `precedes` _        = False  -- Any other are not ordered
