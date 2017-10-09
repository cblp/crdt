{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cv.TPSet
    ( tpSet
    ) where

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (Arbitrary(..), testProperty)

import           CRDT.Cv.TPSet (TPSet (..), add, query, remove)

import           Laws (cvrdtLaws)

instance (Ord a, Arbitrary a) => Arbitrary (TPSet a) where
    arbitrary = TPSet <$> arbitrary <*> arbitrary

tpSet :: TestTree
tpSet = testGroup "TPSet"
    [ cvrdtLaws @(TPSet Int)
    , testProperty "remove" $ \(tpset :: TPSet Int) i ->
        not $ query i (remove i tpset)
    , testProperty "add after remove" $ \(tpset :: TPSet Int) i ->
        not $ query i (add i (remove i tpset))
    ]
