{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module GSet
    ( gSet
    ) where

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty, (==>))

import           CRDT.Cv.GSet (GSet, add, query)

import           Laws (cvrdtLaws)

gSet :: TestTree
gSet = testGroup "GSet"
    [ cvrdtLaws @(GSet Int)
    , testProperty "add" $
        \(set :: GSet Int) i -> not (query i set) ==> query i (add i set)
    ]
