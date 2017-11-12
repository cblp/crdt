{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module ArbitraryOrphans () where

import           Test.QuickCheck (Arbitrary (..), arbitraryBoundedEnum,
                                  elements)

import           CRDT.Cm.Counter (Counter (..))
import           CRDT.Cm.GSet (GSet (..))
import           CRDT.Cm.TwoPSet (TwoPSet (..))
import qualified CRDT.Cm.TwoPSet as TwoPSet
import           CRDT.Cv.ORSet (ORSet (..))
import           CRDT.Cv.GCounter (GCounter (..))
import           CRDT.Cv.LwwElementSet (LwwElementSet (..))
import           CRDT.Cv.Max (Max (..))
import           CRDT.Cv.PNCounter (PNCounter (..))
import qualified CRDT.Cv.TwoPSet as Cv
import           CRDT.LamportClock (LamportTime (..), Pid (..))
import           CRDT.LWW (LWW (..))

instance Arbitrary (Counter a) where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary a => Arbitrary (LWW a) where
    arbitrary = LWW <$> arbitrary <*> arbitrary

deriving instance Arbitrary a => Arbitrary (GSet a)

instance Arbitrary a => Arbitrary (TwoPSet a) where
    arbitrary = elements [TwoPSet.Add, Remove] <*> arbitrary

deriving instance Arbitrary a => Arbitrary (GCounter a)

deriving instance (Arbitrary a, Ord a) => Arbitrary (ORSet a)

deriving instance (Arbitrary a, Ord a) => Arbitrary (LwwElementSet a)

deriving instance Arbitrary a => Arbitrary (Max a)

instance Arbitrary a => Arbitrary (PNCounter a) where
    arbitrary = PNCounter <$> arbitrary <*> arbitrary

deriving instance (Ord a, Arbitrary a) => Arbitrary (Cv.TwoPSet a)

instance Arbitrary LamportTime where
    arbitrary = LamportTime <$> arbitrary <*> arbitrary

deriving instance Arbitrary Pid
