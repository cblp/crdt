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
import           Test.Tasty.QuickCheck (Arbitrary (..), Property, discard,
                                        property, testProperty, (===), (==>))

import           CRDT.Cm (CmRDT (..), concurrent)
import           CRDT.Cv (CvRDT)
import           LamportClock (Timestamp)

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
      , Arbitrary (Intent op), Show (Intent op)
      , Arbitrary (Payload op), Show (Payload op)
      )
    => Property
cmrdtLaw = property $ \state0 i1 i2 t1 t2 -> let
    op1 = updateAtSourceIfCan i1 t1 state0
    op2 = updateAtSourceIfCan i2 t2 state0
    state12 = state0 & updateDownstream op1 & updateDownstream op2
    state21 = state0 & updateDownstream op2 & updateDownstream op1
    in
    concurrent op1 op2 ==> state12 === state21
  where
    updateAtSourceIfCan :: Intent op -> Timestamp -> Payload op -> op
    updateAtSourceIfCan i t state =
        if updateAtSourcePre @op i state then
            updateAtSource i t
        else
            discard
