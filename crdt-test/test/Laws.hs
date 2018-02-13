{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Laws
    ( cmrdtLaw
    , cvrdtLaws
    , opCommutativity
    ) where

import           Control.Monad.State.Strict (MonadState, StateT, evalStateT,
                                             execStateT, get, modify)
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

import           CRDT.Arbitrary ()

semigroupLaw :: forall a . (Arbitrary a, Semigroup a, Eq a, Show a) => TestTree
semigroupLaw = testProperty "associativity" associativity
    where associativity x y (z :: a) = (x <> y) <> z === x <> (y <> z)

semilatticeLaws
    :: forall a . (Arbitrary a, Semilattice a, Eq a, Show a) => [TestTree]
semilatticeLaws =
    [ semigroupLaw @a
    , testProperty "commutativity" commutativity
    , testProperty "idempotency"   idempotency
    ]
  where
    idempotency (x :: a) = x `merge` x === x
    commutativity x (y :: a) = x `merge` y === y `merge` x

cvrdtLaws :: forall a . (Arbitrary a, CvRDT a, Eq a, Show a) => [TestTree]
cvrdtLaws = semilatticeLaws @a

-- | CmRDT law: concurrent ops commute
cmrdtLaw
    :: forall op
     . ( Arbitrary (Intent op)
       , CmRDT op
       , Show (Intent op)
       , Show (Payload op)
       , Show op
       )
    => Property
cmrdtLaw = property concurrentOpsCommute
  where
    concurrentOpsCommute pid1 pid2 pid3 =
        pid1 < pid2 && pid2 < pid3 ==> forAll genFixture $ \case
            Right ((in1, op1), (in2, op2), state3) ->
                concurrent op1 op2
                    ==> opCommutativity (in1, op1) (in2, op2) state3
            Left _ -> discard
      where
        genFixture =
            fmap runIdentity
                .   runGenT
                .   runLamportClockSimT
                $   (,,)
                <$> runProcessSimT pid1 genStateAndTakeLastOp
                <*> runProcessSimT pid2 genStateAndTakeLastOp
                <*> runProcessSimT pid3 genState
        genState              = (`execStateT` initial @op) $ genAndApplyOps @op
        genStateAndTakeLastOp = (`evalStateT` initial @op) $ do
            _ <- genAndApplyOps @op
            genAndApplyOp @op

opCommutativity
    :: forall op
     . (CmRDT op, Show op, Show (Intent op), Show (Payload op))
    => (Intent op, op) -- ^ the op must be made from the intent
    -> (Intent op, op) -- ^ the op must be made from the intent
    -> Payload op -- ^ any reachable state
    -> Property
opCommutativity (in1, op1) (in2, op2) state =
    isJust (makeOp' in1 state)
        ==>  isJust (makeOp' in2 state)
        ==>  counterexample
                 (  show in2
                 ++ " must be valid after "
                 ++ show op1
                 ++ " applied to "
                 ++ show state
                 )
                 (isJust $ makeOp' in2 $ apply op1 state)
        .&&. (apply op1 . apply op2) state
        ===  (apply op2 . apply op1) state
    where makeOp' = makeOp @op @ProcessSim

genAndApplyOp
    :: ( Arbitrary (Intent op)
       , Clock m
       , CmRDT op
       , MonadGen m
       , MonadState (Payload op) m
       )
    => m (Intent op, op)
genAndApplyOp = do
    payload <- get
    intent  <- liftGen arbitrary
    case makeOp intent payload of
        Nothing       -> genAndApplyOp
        Just opAction -> do
            op <- opAction
            modify $ apply op
            pure (intent, op)

genAndApplyOps
    :: ( Arbitrary (Intent op)
       , Clock m
       , CmRDT op
       , MonadGen m
       , MonadState (Payload op) m
       )
    => m [(Intent op, op)]
genAndApplyOps = GenT.listOf genAndApplyOp

instance MonadGen m => MonadGen (ProcessSimT m) where
    liftGen = lift . liftGen
    variant = undefined
    sized f = do
        size <- liftGen getSize
        f size
    resize = undefined
    choose = liftGen . choose

instance MonadGen m => MonadGen (StateT s m) where
    liftGen = lift . liftGen
    variant = undefined
    sized f = do
        size <- liftGen getSize
        f size
    resize = undefined
    choose = liftGen . choose
