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
    :: forall u
    . ( CmRDT u
      , Arbitrary (Op u), Show (Op u)
      , Arbitrary (Payload u), Show (Payload u)
      )
    => Property
cmrdtLaw = property $ \state0 op1 op2 t1 t2 -> let
    up1 = updateAtSourceIfCan t1 op1 state0
    up2 = updateAtSourceIfCan t2 op2 state0
    state12 = state0 & updateDownstream up1 & updateDownstream up2
    state21 = state0 & updateDownstream up2 & updateDownstream up1
    in
    concurrent up1 up2 ==> state12 === state21
  where
    updateAtSourceIfCan :: Timestamp -> Op u -> Payload u -> u
    updateAtSourceIfCan t op state =
        if updateAtSourcePre @u op state then
            updateAtSource t op
        else
            discard
