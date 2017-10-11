{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cv.TwoPSet
    ( twoPSet
    ) where

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           CRDT.Cv.TwoPSet (TwoPSet (..))
import           CRDT.Cv.TwoPSet as TwoPSet

import           Laws (cvrdtLaws)

twoPSet :: TestTree
twoPSet = testGroup "TwoPSet"
    [ cvrdtLaws @(TwoPSet Int)
    -- TODO test addition after and not-after removal
    , testProperty "remove" $ \(s :: TwoPSet Int) i ->
        not . TwoPSet.lookup i $ TwoPSet.remove i s
    , testProperty "remove after add" $ \(s :: TwoPSet Int) i ->
        not . TwoPSet.lookup i . TwoPSet.remove i $ TwoPSet.add i s
    ]
