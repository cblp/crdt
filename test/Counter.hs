{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Counter
    ( counter
    ) where

import           CRDT.Cm.Counter (Counter (..), CounterOp (..))
import           Test.QuickCheck (Arbitrary, arbitrary, arbitraryBoundedEnum)
import           Test.Tasty (TestTree, testGroup)

import           Laws (cmrdtLaw)

instance Arbitrary CounterOp where
    arbitrary = arbitraryBoundedEnum

deriving instance Arbitrary a => Arbitrary (Counter a)

counter :: TestTree
counter = testGroup "Counter" [cmrdtLaw @(Counter Int)]
