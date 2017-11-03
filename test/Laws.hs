{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Laws
    ( cmrdtLaw
    , cvrdtLaws
    ) where

import           Data.Function ((&))
import           Data.Semigroup (Semigroup, (<>))
import           Data.Semilattice (Semilattice, merge)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (Arbitrary (..), Property, property,
                                        testProperty, (===), (==>))

import           CRDT.Cm (CmRDT (..), concurrent)
import           CRDT.Cv (CvRDT)

import           ArbitraryOrphans ()

semigroupLaw :: forall a . (Arbitrary a, Semigroup a, Eq a, Show a) => TestTree
semigroupLaw =
    testGroup "Semigroup law" [testProperty "associativity" associativity]
  where
    associativity (x, y, z :: a) = (x <> y) <> z === x <> (y <> z)

semilatticeLaws
    :: forall a . (Arbitrary a, Semilattice a, Eq a, Show a) => TestTree
semilatticeLaws = testGroup "Semilattice laws"
    [ semigroupLaw @a
    , testProperty "commutativity" commutativity
    , testProperty "idempotency"   idempotency
    ]
  where
    commutativity (x, y :: a) = x `merge` y === y `merge` x
    idempotency (x :: a) = x `merge` x === x

cvrdtLaws :: forall a . (Arbitrary a, CvRDT a, Eq a, Show a) => TestTree
cvrdtLaws = semilatticeLaws @a

-- | CmRDT law: concurrent ops commute
cmrdtLaw
    :: forall op
    . ( CmRDT op
      , Arbitrary op, Show op
      , Arbitrary (Intent op), Show (Intent op)
      , Arbitrary (Payload op), Show (Payload op)
      )
    => Property
cmrdtLaw = property $ \state0 (op1, op2 :: op) -> let
    state12 = state0 & apply op1 & apply op2
    state21 = state0 & apply op2 & apply op1
    in
    concurrent op1 op2 ==> state12 === state21
