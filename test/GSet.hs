{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module GSet
    ( gSet
    ) where

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty, (==>))

import qualified CRDT.Cm.GSet as Cm
import qualified CRDT.Cv.GSet as Cv

import           Laws (cmrdtLaw, cvrdtLaws)

gSet :: TestTree
gSet = testGroup "GSet"
    [ cmrdtLaw @(Cm.GSet Int)
    , cvrdtLaws @(Cv.GSet Int)
    , testProperty "Cv.add" $ \(set :: Cv.GSet Int) i ->
        not (Cv.query i set) ==> Cv.query i (Cv.add i set)
    ]
