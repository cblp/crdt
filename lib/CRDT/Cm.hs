{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module CRDT.Cm
    ( CmRDT (..)
    , concurrent
    ) where

import           Algebra.PartialOrd (PartialOrd (leq))

import           LamportClock (Clock)

-- | TODO(cblp, 2017-09-29) import from lattices >= 1.6
comparable :: PartialOrd a => a -> a -> Bool
comparable a b = a `leq` b || b `leq` a

-- | Not comparable, i. e. ¬(a ≤ b) ∧ ¬(b ≤ a).
concurrent :: PartialOrd a => a -> a -> Bool
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
∀ up1 up2 s .
'concurrent' up1 up2 ==>
    'observe' ('updateDownstream' up1 . 'updateDownstream' up2 $ s) ==
    'observe' ('updateDownstream' up2 . 'updateDownstream' up1 $ s)
@

Idempotency doesn't need to hold.
-}

class (PartialOrd u, Eq (View u)) => CmRDT u where
    type Op       u
    type Payload  u
    type View     u

    -- | Precondition for 'updateAtSource'.
    -- Calculates if the operation is applicable to the current state.
    updateAtSourcePre :: Op u -> Payload u -> Bool
    updateAtSourcePre _ _ = True

    -- | Generate an update to the local and remote replicas.
    -- Doesn't have sense if 'updateAtSourcePre' is false.
    --
    -- May or may not use clock.
    updateAtSource :: Clock m => Op u -> m u
    default updateAtSource :: Applicative m => u -> m u
    updateAtSource = pure

    -- | Apply an update to the payload.
    -- An invalid update must be ignored.
    updateDownstream :: u -> Payload u -> Payload u

    -- | Extract user-visible value from payload
    view :: Payload u -> View u
    default view :: (Payload u ~ View u) => Payload u -> View u
    view = id
