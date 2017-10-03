{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module CRDT.Cm
    ( CmRDT (..)
    , concurrent
    ) where

import           Algebra.PartialOrd (PartialOrd (leq))
import           Data.Observe (Observe (..))

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

class (Observe payload, Eq (Observed payload), PartialOrd update)
        => CmRDT payload op update
        | payload -> op, op -> update, update -> payload
        where

    -- | Precondition for 'updateAtSource'.
    -- Calculates if the operation is applicable to the current state.
    updateAtSourcePre :: op -> payload -> Bool
    updateAtSourcePre _ _ = True

    -- | Generate an update to the local and remote replicas.
    -- Doesn't have sense if 'updateAtSourcePre' is false.
    --
    -- May or may not use clock.
    updateAtSource :: Clock m => op -> m update

    -- | Apply an update to the payload.
    -- An invalid update must be ignored.
    updateDownstream :: update -> payload -> payload
