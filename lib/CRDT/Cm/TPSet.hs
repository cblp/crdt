{-# LANGUAGE NamedFieldPuns #-}

-- | TODO(cblp, 2017-09-29) USet?
module CRDT.Cm.TPSet
    ( initial
    , lookup
    , updateAtSource
    , updateDownstream
    ) where

import           Prelude hiding (lookup)

import           Data.Set (Set)
import qualified Data.Set as Set

newtype TPSet element = TPSet{payload :: Set element}

data Update element = Add element | Remove element

initial :: TPSet element
initial = TPSet Set.empty

-- | query lookup
lookup :: Ord element => element -> TPSet element -> Bool
lookup element TPSet{payload} = Set.member element payload

updateAtSource :: Ord element => Update element -> TPSet element -> Bool
updateAtSource op payload = case op of
    Add _           -> True
    Remove element  -> lookup element payload

updateDownstream :: Ord a => Update a -> TPSet a -> TPSet a
updateDownstream op TPSet{payload} = case op of
    Add element     -> TPSet{payload = Set.insert element payload}
    Remove element  ->
        -- TODO(cblp, 2017-09-29) pre add(e) has been delivered ⊲ Causal order suffices
        TPSet{payload = Set.delete element payload}
