{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module PNCounter
    ( pnCounter
    ) where

import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import qualified CRDT.PNCounter.Cv as Cv
import qualified CRDT.PNCounter.Cv.Internal as Cv

import           GCounter ()
import           Laws (cvrdtLaws)

instance Arbitrary a => Arbitrary (Cv.PNCounter a) where
    arbitrary = Cv.PNCounter <$> arbitrary <*> arbitrary

pnCounter :: TestTree
pnCounter = testGroup "PNCounter"
    [ cvrdtLaws @(Cv.PNCounter Int)
    , testProperty "increment" $
        \(counter :: Cv.PNCounter Int) i ->
            Cv.query (Cv.increment i counter) == succ (Cv.query counter)
    , testProperty "decrement" $
        \(counter :: Cv.PNCounter Int) i ->
            Cv.query (Cv.decrement i counter) == pred (Cv.query counter)
    ]
