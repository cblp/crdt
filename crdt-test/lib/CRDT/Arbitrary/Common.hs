{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module CRDT.Arbitrary.Common () where

import           Test.QuickCheck (Arbitrary (arbitrary))
import           Test.QuickCheck.Instances ()

import           CRDT.LamportClock (LamportTime (LamportTime), Pid (Pid))
import           Data.MultiMap (MultiMap (MultiMap))

deriving instance
        (Arbitrary k, Ord k, Arbitrary v, Ord v) => Arbitrary (MultiMap k v)

instance Arbitrary LamportTime where
    arbitrary = LamportTime <$> arbitrary <*> arbitrary

deriving instance Arbitrary Pid
