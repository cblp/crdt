{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Laws
    ( cmrdtLaw
    , cvrdtLaws
    , gen2
    ) where

import           Control.Monad.State.Strict (StateT, evalStateT)
import           Data.Maybe (fromMaybe)
import           Data.Semigroup (Semigroup, (<>))
import           Data.Semilattice (Semilattice, merge)
import           Test.QuickCheck (Arbitrary (..), Gen, Property, discard,
                                  forAll, property, (===), (==>))
import           Test.Tasty (TestTree)
import           Test.Tasty.QuickCheck (testProperty)

import           CRDT.Cm (CmRDT (..), concurrent)
import           CRDT.Cv (CvRDT)

import           ArbitraryOrphans ()

gen2 :: (StateT s Gen a, s) -> Gen (a, a)
gen2 (gen, start) = evalStateT ((,) <$> gen <*> gen) start

gen3 :: (StateT s Gen a, s) -> Gen (a, a, a)
gen3 (gen, start) = evalStateT ((,,) <$> gen <*> gen <*> gen) start

semigroupLaw
    :: forall a
    . (Arbitrary a, Semigroup a, Eq a, Show a)
    => Maybe (Gen (a, a, a)) -> TestTree
semigroupLaw mgen = testProperty "associativity" $ associativity' mgen
  where
    associativity x y (z :: a) = (x <> y) <> z === x <> (y <> z)
    associativity' = \case
        Nothing  -> property associativity
        Just gen -> forAll gen $ uncurry3 associativity

semilatticeLaws
    :: forall a s
    . (Arbitrary a, Semilattice a, Eq a, Show a)
    => Maybe (StateT s Gen a, s) -> [TestTree]
semilatticeLaws mgen =
    [ semigroupLaw $ gen3 <$> mgen
    , testProperty "commutativity" $ commutativity' $ gen2 <$> mgen
    , testProperty "idempotency"   idempotency
    ]
  where
    idempotency (x :: a) = x `merge` x === x
    commutativity x (y :: a) = x `merge` y === y `merge` x
    commutativity' = \case
        Nothing  -> property commutativity
        Just gen -> forAll gen $ uncurry commutativity

cvrdtLaws
    :: forall a s
    . (Arbitrary a, CvRDT a, Eq a, Show a)
    => Maybe (StateT s Gen a, s) -> [TestTree]
cvrdtLaws = semilatticeLaws

-- | CmRDT law: concurrent ops commute
cmrdtLaw
    :: forall op.
    ( CmRDT op
    , Arbitrary op, Show op
    , Arbitrary (Intent op), Show (Intent op)
    , Arbitrary (Payload op), Show (Payload op)
    )
    => Maybe (Gen (Intent op, Intent op)) -> Property
cmrdtLaw mgen = property $ \(s :: Payload op) ->
    forAll generate $ \(in1, in2) ->
    whenJust (makeOp @op in1 s) $ \op1 ->
    whenJust (makeOp @op in2 s) $ \op2 ->
    concurrent op1 op2 ==>
        (apply op1 . apply op2) s === (apply op2 . apply op1) s
  where
    generate = fromMaybe ((,) <$> arbitrary <*> arbitrary) mgen
    whenJust Nothing  _ = discard
    whenJust (Just a) f = f a

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c
