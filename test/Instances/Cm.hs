{-# OPTIONS_GHC -Wno-orphans #-}

module Instances.Cm () where

import Test.QuickCheck (Arbitrary, arbitrary, arbitraryBoundedEnum)

import CRDT.LWW          (LWW (..))
import CRDT.PNCounter.Cm (PNCounter (..))

instance Arbitrary (PNCounter a) where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary a => Arbitrary (LWW a) where
    arbitrary = Write <$> arbitrary <*> arbitrary
