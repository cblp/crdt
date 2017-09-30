{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module GCounter
    ( gCounter
    ) where

import           Test.QuickCheck (Arbitrary)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           CRDT.Cv.GCounter (GCounter (..), increment, query)

import           Laws (cvrdtLaws)

deriving instance Arbitrary a => Arbitrary (GCounter a)

gCounter :: TestTree
gCounter = testGroup "GCounter"
    [ cvrdtLaws @(GCounter Int)
    , testProperty "increment" $
        \(counter :: GCounter Int) i ->
            query (increment i counter) == succ (query counter)
    ]
