{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Instances.Cv () where

import           Test.QuickCheck (Arbitrary, arbitrary)

import           CRDT.PNCounter.Cv.Internal (PNCounter (..))

import           GCounter ()

instance Arbitrary a => Arbitrary (PNCounter a) where
    arbitrary = PNCounter <$> arbitrary <*> arbitrary
