{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Instances.Cv () where

import           Test.QuickCheck (Arbitrary, arbitrary)

import           CRDT.GCounter.Cv.Internal (GCounter (..))
import           CRDT.PNCounter.Cv.Internal (PNCounter (..))

deriving instance Arbitrary a => Arbitrary (GCounter a)

instance Arbitrary a => Arbitrary (PNCounter a) where
    arbitrary = PNCounter <$> arbitrary <*> arbitrary
