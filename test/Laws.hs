{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Laws
    ( cmrdtLaw
    , cvrdtLaws
    ) where

import           Control.Monad.State.Strict (MonadState, get, modify)
import           Control.Monad.Trans (lift)
import           Data.Functor.Identity (runIdentity)
import           Data.Maybe (isJust)
import           Data.Semigroup (Semigroup, (<>))
import           QuickCheck.GenT (MonadGen, liftGen, runGenT)
import qualified QuickCheck.GenT as GenT
import           Test.QuickCheck (Arbitrary (..), Property, choose,
                                  counterexample, discard, forAll, getSize,
                                  property, (.&&.), (===), (==>))
import           Test.Tasty (TestTree)
import           Test.Tasty.QuickCheck (testProperty)

import           CRDT.Cm (CmRDT (..), concurrent)
import           CRDT.Cv (CvRDT)
import           CRDT.LamportClock (Clock)
import           CRDT.LamportClock.Simulation (ProcessSim, ProcessSimT,
                                               runLamportClockSimT,
                                               runProcessSimT)
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

-- | CmRDT law: concurrent ops commute
cmrdtLaw
    :: forall op.
    ( CmRDT op, Show op
    , Arbitrary (Intent  op), Show (Intent  op)
    , Arbitrary (Payload op), Show (Payload op)
    )
    => Property
cmrdtLaw = property concurrentOpsCommute
  where
    concurrentOpsCommute payload pid1 pid2 pid3 =
        pid1 < pid2 && pid2 < pid3 ==> let
        genOps = fmap runIdentity $ runGenT $ runLamportClockSimT payload $ (,,)
            <$> runProcessSimT pid1 (do
                    _ <- genAndApplyOps @op
                    genAndApplyOp @op)
            <*> runProcessSimT pid2 (do
                    _ <- genAndApplyOps @op
                    genAndApplyOp @op)
            <*> runProcessSimT pid3 (do
                    _ <- genAndApplyOps @op
                    get)
        in
        forAll genOps $ \case
            Right ((in1, op1), (in2, op2), state3) ->
                concurrent op1 op2 ==>
                opCommutativity (in1, op1) (in2, op2) state3
            Left _ -> discard
    opCommutativity (in1, op1) (in2, op2) state =
        isJust (makeOp' in1 state) ==>
        isJust (makeOp' in2 state) ==>
            counterexample
                ( show in1 ++ " must be valid after " ++ show op2 ++
                  " applied to " ++ show state )
                (isJust $ makeOp' in1 $ apply op2 state)
            .&&.
            (apply op1 . apply op2) state === (apply op2 . apply op1) state
    makeOp' = makeOp @op @(ProcessSim (Payload op))

genAndApplyOp
    :: ( CmRDT op, Arbitrary (Intent op)
       , Clock m, MonadState (Payload op) m, MonadGen m
       , Show op, Show (Intent op), Show (Payload op)
       )
    => m (Intent op, op)
genAndApplyOp = do
    payload <- get
    intent <- liftGen arbitrary
    case makeOp intent payload of
        Nothing -> genAndApplyOp
        Just opAction -> do
            op <- opAction
            modify $ apply op
            pure (intent, op)

genAndApplyOps
    :: ( CmRDT op, Arbitrary (Intent op)
       , Clock m, MonadState (Payload op) m, MonadGen m
       , Show op, Show (Intent op), Show (Payload op)
       )
    => m [(Intent op, op)]
genAndApplyOps = GenT.listOf genAndApplyOp

instance MonadGen m => MonadGen (ProcessSimT s m) where
    liftGen = lift . liftGen
    variant = undefined
    sized f = do
        size <- liftGen getSize
        f size
    resize = undefined
    choose = liftGen . choose
