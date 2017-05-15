{-# LANGUAGE TypeFamilies #-}

module CRDT.Cm
    ( CmRDT (..)
    , query
    ) where

import Data.Kind (Type)

{- |
Operation-based, or commutative (Cm) replicated data type.

[Commutativity law]

    @'update' op1 . 'update' op2 == 'update' op2 . 'update' op1@

Idempotency doesn't need to hold.
-}
class CmRDT op where

    -- | The type of the target value
    type State op :: Type

    -- | Apply operation to a value
    update :: op -> State op -> State op

-- | Build state from a series of operations.
query :: (Foldable f, CmRDT op) => f op -> State op -> State op
query ops initial = foldr update initial ops
