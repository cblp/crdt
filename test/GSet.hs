{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module GSet
    ( gSet
    ) where

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (Arbitrary, testProperty, (==>))

import           CRDT.Cm.GSet (GSet, GSetOp (..))
import           CRDT.Cv.GSet (add, query)

import           Laws (cmrdtLaw, cvrdtLaws)

deriving instance Arbitrary a => Arbitrary (GSetOp a)

gSet :: TestTree
gSet = testGroup "GSet"
    [ cmrdtLaw @(GSet Int)
    , cvrdtLaws @(GSet Int)
    , testProperty "Cv.add" $
        \(set :: GSet Int) i -> not (query i set) ==> query i (add i set)
    ]
