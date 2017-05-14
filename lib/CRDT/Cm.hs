{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module CRDT.Cm
    ( CmRDT (..)
    , query
    ) where

import Data.Kind  (Type)
import Data.Proxy (Proxy (..))

{- |
Operation-based, or commutative (Cm) replicated data type.
Laws:
    Commutativity:
        update op1 . update op2 == update op2 . update op1
-}
class CmRDT op where
    type State op :: Type
    update :: op -> State op -> State op
    initial :: Proxy op -> State op

query :: forall f op . (Foldable f, CmRDT op) => f op -> State op
query = foldr update (initial (Proxy :: Proxy op))
