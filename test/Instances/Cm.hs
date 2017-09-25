{-# OPTIONS_GHC -Wno-orphans #-}

module Instances.Cm () where

import           Test.QuickCheck (Arbitrary, arbitrary, arbitraryBoundedEnum)

import           CRDT.GCounter.Cm (GCounter (..))
import qualified CRDT.GCounter.Cm as GCounter
import           CRDT.LWW (LWW (..))
import           CRDT.PNCounter.Cm (PNCounter (..))

instance Arbitrary (GCounter a) where
    arbitrary = pure GCounter.Increment

instance Arbitrary (PNCounter a) where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary a => Arbitrary (LWW a) where
    arbitrary = Write <$> arbitrary <*> arbitrary
