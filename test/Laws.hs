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
import           CRDT.LamportClock (Process, runLamportClock, runProcess)
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

    initialize :: Initial op -> Process (Payload op)
    default initialize
        :: Initial op ~ Payload op => Initial op -> Process (Payload op)
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
    concurrentOpsCommute seed1 seed2 seed3 in1 in2 pid1 pid2 pid3 =
        let genOp seed intent = do
                state <- initialize @op seed
                makeOp @op intent state `orElse` discard
            (op1, op2, s) = runLamportClock $ (,,)
                <$> runProcess pid1 (genOp seed1 in1)
                <*> runProcess pid2 (genOp seed2 in2)
                <*> runProcess pid3 (initialize @op seed3)
        in  concurrent op1 op2 ==> opCommutativity (in1, op1) (in2, op2) s
    opCommutativity (in1, op1) (in2, op2) s =
        isJust (makeOp @op in1 s) ==>
        isJust (makeOp @op in2 s) ==>
            counterexample
                ( show in1 ++ " must be valid after " ++ show op2 ++
                  " applied to " ++ show s )
                (isJust $ makeOp @op in1 $ apply op2 s)
            .&&.
            (apply op1 . apply op2) s === (apply op2 . apply op1) s

orElse :: Maybe a -> a -> a
orElse = flip fromMaybe
