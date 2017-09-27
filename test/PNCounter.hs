{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module PNCounter
    ( pnCounter
    ) where

import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           CRDT.Cv.PNCounter (decrement, increment, query)
import           CRDT.Cv.PNCounter.Internal

import           GCounter ()
import           Laws (cvrdtLaws)

instance Arbitrary a => Arbitrary (PNCounter a) where
    arbitrary = PNCounter <$> arbitrary <*> arbitrary

pnCounter :: TestTree
pnCounter = testGroup "PNCounter"
    [ cvrdtLaws @(PNCounter Int)
    , testProperty "increment" $
        \(counter :: PNCounter Int) i ->
            query (increment i counter) == succ (query counter)
    , testProperty "decrement" $
        \(counter :: PNCounter Int) i ->
            query (decrement i counter) == pred (query counter)
    ]
