{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module PNCounter
    ( pnCounter
    ) where

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           CRDT.Cv.PNCounter (PNCounter (..), decrement, increment, query)

import           GCounter ()
import           Laws (cvrdtLaws)

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
