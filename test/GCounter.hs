{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module GCounter
    ( gCounter
    ) where

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           CRDT.Cv.GCounter (GCounter (..), increment, query)

import           Laws (cvrdtLaws)

gCounter :: TestTree
gCounter = testGroup "GCounter"
    [ cvrdtLaws @(GCounter Int)
    , testProperty "increment" $
        \(counter :: GCounter Int) i ->
            query (increment i counter) == succ (query counter)
    ]
