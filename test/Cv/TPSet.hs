{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cv.TPSet
    ( tpSet
    ) where

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           CRDT.Cv.TPSet (TPSet (..))
import           CRDT.Cv.TPSet as TPSet

import           Laws (cvrdtLaws)

tpSet :: TestTree
tpSet = testGroup "TPSet"
    [ cvrdtLaws @(TPSet Int)
    , testProperty "remove" $ \(tpset :: TPSet Int) i ->
        not $ TPSet.lookup i (TPSet.remove i tpset)
    , testProperty "remove after add" $ \(tpset :: TPSet Int) i ->
        not $ TPSet.lookup i (TPSet.remove i (TPSet.add i tpset))
    ]
