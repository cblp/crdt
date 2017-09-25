{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module PNCounter
    ( pnCounter
    ) where

import           Test.QuickCheck (Arbitrary, arbitrary, arbitraryBoundedEnum)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           CRDT.Cm (update)
import qualified CRDT.PNCounter.Cm as Cm
import qualified CRDT.PNCounter.Cv as Cv
import qualified CRDT.PNCounter.Cv.Internal as Cv

import           GCounter ()
import           Laws (cmrdtLaws, cvrdtLaws)

instance Arbitrary (Cm.PNCounter a) where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary a => Arbitrary (Cv.PNCounter a) where
    arbitrary = Cv.PNCounter <$> arbitrary <*> arbitrary

pnCounter :: TestTree
pnCounter = testGroup "PNCounter"
    [ testGroup "Cv"
        [ cvrdtLaws @(Cv.PNCounter Int)
        , testProperty "increment" $
            \(counter :: Cv.PNCounter Int) i ->
                Cv.query (Cv.increment i counter) == succ (Cv.query counter)
        , testProperty "decrement" $
            \(counter :: Cv.PNCounter Int) i ->
                Cv.query (Cv.decrement i counter) == pred (Cv.query counter)
        ]
    , testGroup "Cm"
        [ cmrdtLaws @(Cm.PNCounter Int)
        , testProperty "increment" $
            \(counter :: Int) -> update Cm.Increment counter == succ counter
        , testProperty "decrement" $
            \(counter :: Int) -> update Cm.Decrement counter == pred counter
        ]
    ]
