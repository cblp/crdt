{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

-- | TODO(cblp, 2017-09-29) USet?
module CRDT.Cm.TwoPSet
    ( TwoPSet (..)
    ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           CRDT.Cm (CausalOrd (..), CmRDT (..))

data TwoPSet a = Add a | Remove a
    deriving (Eq, Show)

instance Ord a => CmRDT (TwoPSet a) where
    type Payload (TwoPSet a) = Map a Bool

    initial = Map.empty

    makeOp op payload = case op of
        Add    _ -> Just $ pure op
        Remove a
            | isKnown a -> Just $ pure op
            | otherwise -> Nothing
      where
        isKnown a = Map.member a payload

    apply = \case
        Add    a -> Map.insertWith (&&) a True
        Remove a -> Map.insert          a False

instance Eq a => CausalOrd (TwoPSet a) where
    Add b `precedes` Remove a = a == b -- `Remove e` can occur only after `Add e`
    _     `precedes` _        = False  -- Any other are not ordered
