{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Counter
    ( counter
    ) where

import           CRDT.Cm.Counter (Counter)
import           Test.Tasty (TestTree, testGroup)

import           Laws (cmrdtLaw)

counter :: TestTree
counter = testGroup "Counter" [cmrdtLaw @(Counter Int)]
