{-# LANGUAGE TypeApplications #-}

module Cm.TwoPSet
    ( twoPSet
    ) where

import           CRDT.Cm.TwoPSet (TwoPSet)
import           Test.Tasty (TestTree, testGroup)

import           Laws (cmrdtLaw)

twoPSet :: TestTree
twoPSet = testGroup "TwoPSet" [cmrdtLaw @(TwoPSet Int)]
