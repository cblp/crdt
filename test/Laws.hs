{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Laws
    ( cmrdtLaw
    , cvrdtLaws
    ) where

import           Control.Monad ((<=<))
import           Data.Maybe (fromMaybe, isJust)
import           Data.Semigroup (Semigroup, (<>))
import           Test.QuickCheck (Arbitrary (..), Property, counterexample,
                                  discard, property, (.&&.), (===), (==>))
import           Test.Tasty (TestTree)
import           Test.Tasty.QuickCheck (testProperty)

import           CRDT.Cm (CmRDT (..), concurrent)
import           CRDT.Cm.Counter (Counter)
import           CRDT.Cm.GSet (GSet)
import           CRDT.Cm.TwoPSet (TwoPSet)
import           CRDT.Cv (CvRDT)
import           CRDT.LamportClock (Clock, ProcessSim, runLamportClockSim,
                                    runProcessSim)
import           CRDT.LWW (LWW)
import qualified CRDT.LWW as LWW
import           Data.Semilattice (Semilattice, merge)

import           ArbitraryOrphans ()

semigroupLaw :: forall a. (Arbitrary a, Semigroup a, Eq a, Show a) => TestTree
semigroupLaw = testProperty "associativity" associativity
  where
    associativity x y (z :: a) = (x <> y) <> z === x <> (y <> z)

semilatticeLaws
    :: forall a. (Arbitrary a, Semilattice a, Eq a, Show a) => [TestTree]
semilatticeLaws =
    [ semigroupLaw @a
    , testProperty "commutativity" commutativity
    , testProperty "idempotency"   idempotency
    ]
  where
    idempotency (x :: a) = x `merge` x === x
    commutativity x (y :: a) = x `merge` y === y `merge` x

cvrdtLaws :: forall a. (Arbitrary a, CvRDT a, Eq a, Show a) => [TestTree]
cvrdtLaws = semilatticeLaws @a

class Initialize op where
    type Initial op
    type Initial op = Payload op

    initialize :: Clock m => Initial op -> m (Payload op)
    default initialize
        :: (Applicative m, Initial op ~ Payload op)
        => Initial op -> m (Payload op)
    initialize = pure

instance Initialize (Counter a)

instance Initialize (GSet a)

instance Initialize (LWW a) where
    type Initial (LWW a) = a
    initialize = LWW.initial

instance Initialize (TwoPSet a)

-- | CmRDT law: concurrent ops commute
cmrdtLaw
    :: forall op.
    ( CmRDT op
    , Arbitrary op, Show op
    , Arbitrary (Intent  op), Show (Intent  op)
    , Arbitrary (Payload op), Show (Payload op)
    , Initialize op
    , Arbitrary (Initial op), Show (Initial op)
    )
    => Property
cmrdtLaw = property concurrentOpsCommute
  where
    concurrentOpsCommute st1 st2 seed3 in1 in2 pid1 pid2 pid3 =
        let (op1, op2, state) = runLamportClockSim $ (,,)
                <$> runProcessSim pid1 (makeOp @op in1 st1 `orElse` discard)
                <*> runProcessSim pid2 (makeOp @op in2 st2 `orElse` discard)
                <*> runProcessSim pid3 (initialize @op seed3)
        in  concurrent op1 op2 ==> opCommutativity (in1, op1) (in2, op2) state
    opCommutativity (in1, op1) (in2, op2) state =
        isJust (makeOp @op @ProcessSim in1 state) ==>
        isJust (makeOp @op @ProcessSim in2 state) ==>
            counterexample
                ( show in1 ++ " must be valid after " ++ show op2 ++
                  " applied to " ++ show state )
                (isJust $ makeOp @op @ProcessSim in1 =<< apply op2 state)
            .&&.
            (apply op1 <=< apply op2) state === (apply op2 <=< apply op1) state

orElse :: Maybe a -> a -> a
orElse = flip fromMaybe
