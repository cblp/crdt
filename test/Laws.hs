{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Laws
    ( cmrdtLaws
    , cvrdtLaws
    , semigroupLaw
    , semilatticeLaws
    ) where

import           Data.Semigroup (Semigroup, (<>))
import           Data.Semilattice (Semilattice, merge)
import           Test.QuickCheck (Arbitrary)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           CRDT.Cm (CmRDT, State, update)
import           CRDT.Cv (CvRDT)

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

cmrdtLaws
    :: forall op
    . ( Arbitrary op, CmRDT op, Show op
      , Arbitrary (State op), Eq (State op), Show (State op)
      )
    => TestTree
cmrdtLaws = testProperty "CmRDT law: commutativity" commutativity
  where
    commutativity :: op -> op -> State op -> Bool
    commutativity op1 op2 x =
        (update op1 . update op2) x == (update op2 . update op1) x
