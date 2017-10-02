{-# LANGUAGE TypeApplications #-}

module TPSet
    ( tpSet
    ) where

import           CRDT.Cm.TPSet (TPSet)
import           Test.Tasty (TestTree, testGroup)

import           Laws (cmrdtLaw)

tpSet :: TestTree
tpSet = testGroup "TPSet" [cmrdtLaw @(TPSet Int)]
