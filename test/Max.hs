{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Max
    ( maxTest
    ) where

import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           CRDT.Max (Max, point, query)
import           Data.Semilattice (merge)

import           Laws (cvrdtLaws)

instance Arbitrary a => Arbitrary (Max a) where
    arbitrary = point <$> arbitrary

maxTest :: TestTree
maxTest = testGroup "Max"
    [ testGroup "Cv"
        [ cvrdtLaws @(Max Int)
        , testProperty "merge" $
            \(m :: Max Int) i -> query (point i `merge` m) == max i (query m)
        ]
    ]
