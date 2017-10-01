{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Max
    ( maxTest
    ) where

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           CRDT.Cv.Max (Max, point, query)
import           Data.Semilattice (merge)

import           Laws (cvrdtLaws)

maxTest :: TestTree
maxTest = testGroup "Max"
    [ testGroup "Cv"
        [ cvrdtLaws @(Max Int)
        , testProperty "merge" $
            \(m :: Max Int) i -> query (point i `merge` m) == max i (query m)
        ]
    ]
