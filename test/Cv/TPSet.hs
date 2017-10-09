{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cv.TPSet
    ( tpSet
    ) where

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (Arbitrary(..), testProperty, (==>))

import           CRDT.Cv.TPSet (TPSet (..)
                               , add, initial
                               , query, remove)

import           Laws (cvrdtLaws)

instance (Ord a, Arbitrary a) => Arbitrary (TPSet a) where
    arbitrary = TPSet <$> arbitrary <*> arbitrary

tpSet :: TestTree
tpSet = testGroup "TPSet"
    [ cvrdtLaws @(TPSet Int)
    , testProperty "remove" $ \(tpSet :: TPSet Int) i ->
        query i (remove i tpSet) == False
    , testProperty "add after remove" $ \(tpSet :: TPSet Int) i ->
        query i (add i (remove i tpSet)) == False
    ]
