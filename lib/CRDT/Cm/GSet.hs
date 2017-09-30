{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module CRDT.Cm.GSet
    ( GSet
    , GSetOp (..)
    , initial
    , lookup
    ) where

import           Prelude hiding (lookup)

import           Algebra.PartialOrd (PartialOrd (leq))
import           Data.Set (Set)
import qualified Data.Set as Set

import           CRDT.Cm (CmRDT (..), Update (..))
import           CRDT.Cv.GSet (GSet)

newtype GSetOp element = Add element
    deriving (Eq, Show)

initial :: GSet element
initial = Set.empty

-- | query lookup
lookup :: Ord element => element -> GSet element -> Bool
lookup = Set.member

instance Ord a => CmRDT (Set a) where
    type Op (Set a) = GSetOp a

    update (Add element) =
        Update{atSource = id, downstream = Set.insert element}

    type Query (Set a) = Set a

    query = id

instance Eq a => PartialOrd (GSetOp a) where
    leq _ _ = False
