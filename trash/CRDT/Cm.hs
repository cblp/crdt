{-# LANGUAGE TypeFamilies #-}

module CRDT.Cm
    ( CmRDT (..)
    ) where

import           Data.Kind (Type)

{- |
Operation-based, or commutative (Cm) replicated data type.

Values of the type must be "operations",
i. e. they must be applicable to the state via the 'update' method.

[Commutativity law]

    @'update' op1 . 'update' op2 == 'update' op2 . 'update' op1@

Idempotency doesn't need to hold.
-}
class CmRDT op where

    -- | The type of the target value
    type State op :: Type

    -- | Apply operation to a value
    update :: op -> State op -> State op

cmrdtLaws
    :: forall op
    . ( Arbitrary op, CmRDT op, Show op
      , Arbitrary (State op), Eq (State op), Show (State op)
      )
    => TestTree
cmrdtLaws = testProperty "CmRDT law: commutativity" commutativity
  where
    commutativity :: op -> op -> State op -> Bool
    commutativity op1 op2 x =
        (update op1 . update op2) x == (update op2 . update op1) x
