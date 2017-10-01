{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Laws
    ( cmrdtLaw
    , cvrdtLaws
    ) where

import           Data.Function ((&))
import           Data.Semigroup (Semigroup, (<>))
import           Data.Semilattice (Semilattice, merge)
import           Test.QuickCheck (Arbitrary)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty, (==>))

import           CRDT.Cm (CmRDT (..), concurrent)
import           CRDT.Cv (CvRDT)
import           LamportClock (runLamportClock, runProcess)

import           ArbitraryOrphans ()

semigroupLaw :: forall a . (Arbitrary a, Semigroup a, Eq a, Show a) => TestTree
semigroupLaw =
    testGroup "Semigroup law" [testProperty "associativity" associativity]
  where
    associativity :: a -> a -> a -> Bool
    associativity x y z = (x <> y) <> z == x <> (y <> z)

semilatticeLaws
    :: forall a . (Arbitrary a, Semilattice a, Eq a, Show a) => TestTree
semilatticeLaws = testGroup "Semilattice laws"
    [ semigroupLaw @a
    , testProperty "commutativity" commutativity
    , testProperty "idempotency"   idempotency
    ]
  where
    commutativity :: a -> a -> Bool
    commutativity x y = x `merge` y == y `merge` x

    idempotency :: a -> Bool
    idempotency x = x `merge` x == x

cvrdtLaws :: forall a . (Arbitrary a, CvRDT a, Eq a, Show a) => TestTree
cvrdtLaws = semilatticeLaws @a

cmrdtLaw
    :: forall state op up view
    . ( CmRDT state op up view, Arbitrary state, Show state
      , Arbitrary op, Show op
      )
    => TestTree
cmrdtLaw = testProperty "CmRDT law: concurrent ops commute" $
    \(state0 :: state) (op1 :: op) (op2 :: op) pid ->
        runLamportClock $ runProcess pid $ do
            up1 <- updateAtSource op1
            up2 <- updateAtSource op2
            let state12 = state0 & updateDownstream up1 & updateDownstream up2
            let state21 = state0 & updateDownstream up2 & updateDownstream up1
            pure $ concurrent up1 up2 ==> view state12 == view state21
