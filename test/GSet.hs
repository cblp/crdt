{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module GSet
    ( gSet
    ) where

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty, (==>))

import           CRDT.Cm.GSet (GSet)
import           CRDT.Cv.GSet (add, query)

import           Laws (cmrdtLaw, cvrdtLaws)

gSet :: TestTree
gSet = testGroup "GSet"
    [ cmrdtLaw @(GSet Int)
    , cvrdtLaws @(GSet Int)
    , testProperty "Cv.add" $
        \(set :: GSet Int) i -> not (query i set) ==> query i (add i set)
    ]
