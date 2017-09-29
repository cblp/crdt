{-# LANGUAGE NamedFieldPuns #-}

module CRDT.Cm.GSet
    ( GSet
    , Update
    , initial
    , lookup
    , updateAtSource
    , updateDownstream
    ) where

import           Prelude hiding (lookup)

import           Data.Set (Set)
import qualified Data.Set as Set

newtype GSet element = GSet{payload :: Set element}

newtype Update element = Add element

initial :: GSet element
initial = GSet Set.empty

-- | query lookup
lookup :: Ord element => element -> GSet element -> Bool
lookup element GSet{payload} = Set.member element payload

updateAtSource :: ()
updateAtSource = ()

updateDownstream :: Ord a => Update a -> GSet a -> GSet a
updateDownstream (Add element) GSet{payload} =
    GSet{payload = Set.insert element payload}
