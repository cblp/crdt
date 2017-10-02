{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module ArbitraryOrphans () where

import           Test.QuickCheck (Arbitrary (..), arbitraryBoundedEnum, oneof)

import           CRDT.Cm.Counter (Counter (..), CounterOp (..))
import           CRDT.Cm.GSet (Add (..))
import           CRDT.Cm.LWW (Assign (..), LWW (..))
import           CRDT.Cm.TPSet (TPSet (..), TPSetOp (..))
import qualified CRDT.Cm.TPSet as TPSet
import           CRDT.Cv.GCounter (GCounter (..))
import           CRDT.Cv.Max (Max (..))
import           CRDT.Cv.PNCounter (PNCounter (..))
import           LamportClock (Pid (..), Timestamp (..))

deriving instance Arbitrary a => Arbitrary (Counter a)

instance Arbitrary (CounterOp a) where
    arbitrary = arbitraryBoundedEnum

deriving instance Arbitrary a => Arbitrary (Add a)

deriving instance Arbitrary a => Arbitrary (Assign a)

instance Arbitrary a => Arbitrary (LWW a) where
    arbitrary = LWW <$> arbitrary <*> arbitrary

deriving instance (Arbitrary a, Ord a) => Arbitrary (TPSet a)

instance Arbitrary a => Arbitrary (TPSetOp a) where
    arbitrary = oneof [TPSet.Add <$> arbitrary, Remove <$> arbitrary]

deriving instance Arbitrary a => Arbitrary (GCounter a)

deriving instance Arbitrary a => Arbitrary (Max a)

instance Arbitrary a => Arbitrary (PNCounter a) where
    arbitrary = PNCounter <$> arbitrary <*> arbitrary

deriving instance Arbitrary Pid

instance Arbitrary Timestamp where
    arbitrary = Timestamp <$> arbitrary <*> arbitrary
