{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

-- | TODO(cblp, 2017-09-29) USet?
module CRDT.Cm.TPSet
    ( TPSet (..)
    , updateAtSource
    , updateDownstream
    ) where

import           Algebra.PartialOrd (PartialOrd (..))
import           Data.Set (Set)
import qualified Data.Set as Set

import           CRDT.Cm (CmRDT (..))

data TPSet a = Add a | Remove a
    deriving (Eq, Show)

instance Ord a => CmRDT (TPSet a) where
    type Op       (TPSet a) = TPSet a
    type Payload  (TPSet a) = Set a
    type View     (TPSet a) = Set a

    updateAtSourcePre op payload = case op of
        Add _     -> True
        Remove a  -> Set.member a payload

    updateDownstream = \case
        Add a     -> Set.insert a
        Remove a  -> Set.delete a

instance Eq a => PartialOrd (TPSet a) where
    leq (Remove a) (Add b) = a == b -- `Remove e` can occur only after `Add e`
    leq _ _ = False -- Any other are not ordered
