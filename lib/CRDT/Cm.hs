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
a __payload__, an __operation__ (@op@) and an __update__.

[Payload]
    Internal state of a replica.
[Operation]
    User's request to update.
[Update]
    Operation to be applied to other replicas.

For many types /operation/ and /update/ may be the same.
But for 'CRDT.Cm.LWW.LWW', for instance, this rule doesn't hold:
user can request only value, and type attaches a timestamp to it.

== Additional constraint — commutativity law

Concurrent updates are observed equally.

@
∀ up1 up2 .
'concurrent' up1 up2 ==>
    'updateDownstream' up1 . 'updateDownstream' up2 ==
    'updateDownstream' up2 . 'updateDownstream' up1
@

Idempotency doesn't need to hold.
-}

class (CausalOrd u, Eq (Payload u)) => CmRDT u where
    type Op u
    type Op u = u -- default case

    type Payload u

    -- | Precondition for 'updateAtSource'.
    -- Calculates if the operation is applicable to the current state.
    updateAtSourcePre :: Op u -> Payload u -> Bool
    updateAtSourcePre _ _ = True

    -- | Generate an update to the local and remote replicas.
    -- Doesn't have sense if 'updateAtSourcePre' is false.
    --
    -- May or may not use clock.
    updateAtSource :: Timestamp -> Op u -> u

    default updateAtSource :: (Op u ~ u) => Timestamp -> Op u -> u
    updateAtSource _ = id

    -- | Apply an update to the payload.
    -- An invalid update must be ignored.
    updateDownstream :: u -> Payload u -> Payload u
