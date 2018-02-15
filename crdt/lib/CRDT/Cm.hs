{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module CRDT.Cm
    ( CausalOrd (..)
    , CmRDT (..)
    , concurrent
    , query
    , makeAndApplyOp
    , makeAndApplyOps
    ) where

import           Prelude hiding (fail)

import           Control.Monad.Fail (MonadFail, fail)
import           Control.Monad.State.Strict (MonadState, get, modify)

import           CRDT.LamportClock (Clock)

-- | Partial order for causal semantics.
-- Values of some type may be ordered and causally-ordered different ways.
class CausalOrd a where
    -- | @x `precedes` y@ means that
    -- @x@ must go before @y@ and @y@ can not go before @x@.
    precedes :: a -> a -> Bool

comparable :: CausalOrd a => a -> a -> Bool
comparable a b = a `precedes` b || b `precedes` a

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
'concurrent' op1 op2 ==> 'apply' op1 . 'apply' op2 == 'apply' op2 . 'apply' op1
@

Idempotency doesn't need to hold.
-}

class (CausalOrd op, Eq (Payload op)) => CmRDT op where
    type Intent op
    type Intent op = op -- common case

    type Payload op

    initial :: Payload op

    -- | Generate an update to the local and remote replicas.
    --
    -- Returns 'Nothing' if the intended operation is not applicable.
    makeOp :: Clock m => Intent op -> Payload op -> Maybe (m op)

    default makeOp
        :: (Intent op ~ op, Applicative m)
        => Intent op -> Payload op -> Maybe (m op)
    makeOp i _ = Just $ pure i

    -- | Apply an update to the payload (downstream).
    -- An invalid update must be ignored.
    --
    -- TODO(Syrovetsky, 2017-12-05) There is no downstream precondition yet.
    -- We must make a test for it first.
    apply :: op -> Payload op -> Payload op

query :: forall op f . (CmRDT op, Foldable f) => f op -> Payload op
query = foldl (flip apply) (initial @op)

-- | Make op and apply it to the payload -- a common routine at the source node.
makeAndApplyOp
    :: (CmRDT op, Clock m, MonadFail m, MonadState (Payload op) m)
    => Intent op
    -> m op
makeAndApplyOp intent = do
    payload <- get
    case makeOp intent payload of
        Nothing       -> fail "precodition failed"
        Just opAction -> do
            op <- opAction
            modify $ apply op
            pure op

makeAndApplyOps
    :: ( CmRDT op
       , Clock m
       , MonadFail m
       , MonadState (Payload op) m
       , Traversable f
       )
    => f (Intent op)
    -> m (f op)
makeAndApplyOps = traverse makeAndApplyOp
