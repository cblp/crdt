{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Instances.Cv () where

import           Data.Vector     (Vector)
import qualified Data.Vector     as Vector
import           Test.QuickCheck (Arbitrary, arbitrary)

import CRDT.GCounter.Cv.Internal  (GCounter (..))
import CRDT.PNCounter.Cv.Internal (PNCounter (..))

instance Arbitrary a => Arbitrary (Vector a) where
    arbitrary = Vector.fromList <$> arbitrary

deriving instance Arbitrary a => Arbitrary (GCounter a)

instance Arbitrary a => Arbitrary (PNCounter a) where
    arbitrary = PNCounter <$> arbitrary <*> arbitrary
