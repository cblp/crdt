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

[Commutativity law]

    @'update' op1 . 'update' op2 == 'update' op2 . 'update' op1@
-}
class CmRDT op where

    -- | The type of the target value
    type State op :: Type

    -- | Apply operation to a value
    update :: op -> State op -> State op

    -- | Initial state
    initial :: Proxy op -> State op

-- | Build state from a series of operations.
query :: forall f op . (Foldable f, CmRDT op) => f op -> State op
query = foldr update (initial (Proxy :: Proxy op))
