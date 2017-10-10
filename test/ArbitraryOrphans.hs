{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module ArbitraryOrphans () where

import           Test.Tasty.QuickCheck (Arbitrary (..), arbitraryBoundedEnum,
                                        oneof)

import           CRDT.Cm.Counter (Counter (..))
import           CRDT.Cm.GSet (GSet (..))
import           CRDT.Cm.TPSet (TPSet (..))
import qualified CRDT.Cm.TPSet as TPSet
import           CRDT.Cv.GCounter (GCounter (..))
import           CRDT.Cv.Max (Max (..))
import           CRDT.Cv.PNCounter (PNCounter (..))
import qualified CRDT.Cv.TPSet as Cv
import           CRDT.LWW (Assign (..), LWW (..))
import           LamportClock (Pid (..), Timestamp (..))

instance Arbitrary (Counter a) where
    arbitrary = arbitraryBoundedEnum

deriving instance Arbitrary a => Arbitrary (Assign a)

instance Arbitrary a => Arbitrary (LWW a) where
    arbitrary = LWW <$> arbitrary <*> arbitrary

deriving instance Arbitrary a => Arbitrary (GSet a)

instance Arbitrary a => Arbitrary (TPSet a) where
    arbitrary = oneof [TPSet.Add <$> arbitrary, Remove <$> arbitrary]

deriving instance Arbitrary a => Arbitrary (GCounter a)

deriving instance Arbitrary a => Arbitrary (Max a)

instance Arbitrary a => Arbitrary (PNCounter a) where
    arbitrary = PNCounter <$> arbitrary <*> arbitrary

deriving instance Arbitrary Pid

instance Arbitrary Timestamp where
    arbitrary = Timestamp <$> arbitrary <*> arbitrary

instance (Ord a, Arbitrary a) => Arbitrary (Cv.TPSet a) where
    arbitrary = Cv.TPSet <$> arbitrary <*> arbitrary
