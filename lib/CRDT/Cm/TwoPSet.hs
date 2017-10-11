{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

-- | TODO(cblp, 2017-09-29) USet?
module CRDT.Cm.TwoPSet
    ( TwoPSet (..)
    , updateAtSource
    , updateDownstream
    ) where

import           Data.Set (Set)
import qualified Data.Set as Set

import           CRDT.Cm (CausalOrd (..), CmRDT (..))

data TwoPSet a = Add a | Remove a
    deriving (Eq, Show)

instance Ord a => CmRDT (TwoPSet a) where
    type Payload  (TwoPSet a) = Set a

    updateAtSourcePre op payload = case op of
        Add _     -> True
        Remove a  -> Set.member a payload

    updateDownstream = \case
        Add a     -> Set.insert a
        Remove a  -> Set.delete a

instance Eq a => CausalOrd (TwoPSet a) where
    Remove a `before` Add b = a == b -- `Remove e` can occur only after `Add e`
    _        `before` _     = False  -- Any other are not ordered
