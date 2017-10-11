{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module ArbitraryOrphans () where

import           Test.Tasty.QuickCheck (Arbitrary (..), arbitraryBoundedEnum,
                                        oneof)

import           CRDT.Cm.Counter (Counter (..))
import           CRDT.Cm.GSet (GSet (..))
import           CRDT.Cm.TwoPSet (TwoPSet (..))
import qualified CRDT.Cm.TwoPSet as TwoPSet
import           CRDT.Cv.GCounter (GCounter (..))
import           CRDT.Cv.Max (Max (..))
import           CRDT.Cv.PNCounter (PNCounter (..))
import qualified CRDT.Cv.TwoPSet as Cv
import           CRDT.LWW (Assign (..), LWW (..))
import           LamportClock (Pid (..), Timestamp (..))

instance Arbitrary (Counter a) where
    arbitrary = arbitraryBoundedEnum

deriving instance Arbitrary a => Arbitrary (Assign a)

instance Arbitrary a => Arbitrary (LWW a) where
    arbitrary = LWW <$> arbitrary <*> arbitrary

deriving instance Arbitrary a => Arbitrary (GSet a)

instance Arbitrary a => Arbitrary (TwoPSet a) where
    arbitrary = oneof [TwoPSet.Add <$> arbitrary, Remove <$> arbitrary]

deriving instance Arbitrary a => Arbitrary (GCounter a)

deriving instance Arbitrary a => Arbitrary (Max a)

instance Arbitrary a => Arbitrary (PNCounter a) where
    arbitrary = PNCounter <$> arbitrary <*> arbitrary

deriving instance Arbitrary Pid

instance Arbitrary Timestamp where
    arbitrary = Timestamp <$> arbitrary <*> arbitrary

instance (Ord a, Arbitrary a) => Arbitrary (Cv.TwoPSet a) where
    arbitrary = Cv.TwoPSet <$> arbitrary <*> arbitrary
