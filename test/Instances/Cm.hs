{-# OPTIONS_GHC -Wno-orphans #-}

module Instances.Cm () where

import           Test.QuickCheck (Arbitrary, arbitrary)

import           CRDT.LWW (LWW (..))

instance Arbitrary a => Arbitrary (LWW a) where
    arbitrary = Write <$> arbitrary <*> arbitrary
