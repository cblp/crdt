{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Laws
    ( cmrdtLaw
    , cvrdtLaws
    ) where

import           Control.Monad (unless)
import           Data.Function ((&))
import           Data.Semigroup (Semigroup, (<>))
import           Data.Semilattice (Semilattice, merge)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (Arbitrary (..), discard, testProperty,
                                        (===), (==>))

import           CRDT.Cm (CmRDT (..), concurrent)
import           CRDT.Cv (CvRDT)
import           LamportClock (Pid, runLamportClock, runProcess)

import           ArbitraryOrphans ()

semigroupLaw :: forall a . (Arbitrary a, Semigroup a, Eq a, Show a) => TestTree
semigroupLaw =
    testGroup "Semigroup law" [testProperty "associativity" associativity]
  where
    associativity (x, y, z :: a) = (x <> y) <> z == x <> (y <> z)

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

data CmrdtLawParams payload op = CmrdtLawParams
    { pid :: Pid
    , state0 :: payload
    , op1, op2 :: op
    }
    deriving (Show)

instance (Arbitrary payload, Arbitrary op)
        => Arbitrary (CmrdtLawParams payload op) where
    arbitrary =
        CmrdtLawParams <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

cmrdtLaw
    :: forall payload op up view
    . ( CmRDT payload op up view, Arbitrary payload, Show payload
      , Arbitrary op, Show op
      , Show view
      )
    => TestTree
cmrdtLaw = testProperty "CmRDT law: concurrent ops commute" $
    \(CmrdtLawParams{pid, state0, op1, op2} :: CmrdtLawParams payload op) ->
        runLamportClock $ runProcess pid $ do
            up1 <- updateAtSource' op1 state0
            up2 <- updateAtSource' op2 state0
            let state12 = state0 & updateDownstream up1 & updateDownstream up2
            let state21 = state0 & updateDownstream up2 & updateDownstream up1
            pure $ concurrent up1 up2 ==> view state12 === view state21
  where
    updateAtSource' op state = do
        unless (updateAtSourcePre op state) discard
        updateAtSource op
