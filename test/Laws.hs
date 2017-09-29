{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import           CRDT.Cm (CmRDT (..), concurrent, query, updateLocally)
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

cmrdtLaw
    :: forall a
    . (CmRDT a, Arbitrary a, Show a, Arbitrary (Op a), Show (Op a)) => TestTree
cmrdtLaw = testProperty "CmRDT law: concurrent ops commute" $
    \(state0 :: a) op1 op2 -> let
        state12 = state0 & updateLocally op1 & updateLocally op2
        state21 = state0 & updateLocally op2 & updateLocally op1
        in
        concurrent op1 op2 ==> query state12 == query state21
