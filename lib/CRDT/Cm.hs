{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module CRDT.Cm
    ( CausalOrd (..)
    , CmRDT (..)
    , concurrent
    ) where

import           LamportClock (Timestamp)

-- | Partial order for causal semantics.
-- Values of some type may be ordered and causally-ordered different ways.
class CausalOrd a where
    before :: a -> a -> Bool

comparable :: CausalOrd a => a -> a -> Bool
comparable a b = a `before` b || b `before` a

-- | Not comparable, i. e. ¬(a ≤ b) ∧ ¬(b ≤ a).
concurrent :: CausalOrd a => a -> a -> Bool
concurrent a b = not $ comparable a b

{- |
Operation-based, or commutative (Cm) replicated data type.

== Implementation

In Haskell, a CmRDT implementation consists of 3 types —
a __payload__, an __operation__ (@op@) and an __intent__.

[Payload]
    Internal state of a replica.
[Intent]
    User's request to update.
[Operation (Op)]
    Operation to be applied to other replicas.

For many types /operation/ and /intent/ may be the same.
But for 'CRDT.Cm.LWW.LWW', for instance, this rule doesn't hold:
user can request only value, and type attaches a timestamp to it.

== Additional constraint — commutativity law

Concurrent updates are observed equally.

@
∀ op1 op2 .
'concurrent' op1 op2 ==>
    'updateDownstream' op1 . 'updateDownstream' op2 ==
    'updateDownstream' op2 . 'updateDownstream' op1
@

Idempotency doesn't need to hold.
-}

class (CausalOrd op, Eq (Payload op)) => CmRDT op where
    type Intent op
    type Intent op = op -- default case

    type Payload op

    -- | Precondition for 'updateAtSource'.
    -- Calculates if the operation is applicable to the current state.
    updateAtSourcePre :: Intent op -> Payload op -> Bool
    updateAtSourcePre _ _ = True

    -- | Generate an update to the local and remote replicas.
    -- Doesn't have sense if 'updateAtSourcePre' is false.
    --
    -- May or may not use clock.
    updateAtSource :: Timestamp -> Intent op -> op

    default updateAtSource :: (Intent op ~ op) => Timestamp -> Intent op -> op
    updateAtSource _ = id

    -- | Apply an update to the payload.
    -- An invalid update must be ignored.
    updateDownstream :: op -> Payload op -> Payload op
