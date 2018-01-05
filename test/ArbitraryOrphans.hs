{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}

module ArbitraryOrphans () where

import           Test.QuickCheck (Arbitrary (..), arbitraryBoundedEnum,
                                  elements)
import           Test.QuickCheck.Gen (Gen (MkGen))
import           Test.QuickCheck.Instances ()
import           Test.QuickCheck.Random (mkQCGen)

import           CRDT.Cm.Counter (Counter (..))
import           CRDT.Cm.GSet (GSet (..))
import qualified CRDT.Cm.TwoPSet as Cm
import           CRDT.Cv.GCounter (GCounter (..))
import           CRDT.Cv.LwwElementSet (LwwElementSet (..))
import           CRDT.Cv.ORSet (ORSet (..))
import           CRDT.Cv.PNCounter (PNCounter (..))
import qualified CRDT.Cv.TwoPSet as Cv
import           CRDT.LamportClock (LamportTime (..), Pid (..))
import           CRDT.LWW (LWW (..))

instance Arbitrary (Counter a) where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary a => Arbitrary (LWW a) where
    arbitrary = do
        time <- arbitrary
        value <- seeded (hash time) arbitrary
        pure LWW{value, time}
      where
        hash (LamportTime t (Pid p)) = fromIntegral t * 997 + fromIntegral p

deriving instance Arbitrary a => Arbitrary (GSet a)

instance Arbitrary a => Arbitrary (Cm.TwoPSet a) where
    arbitrary = elements [Cm.Add, Cm.Remove] <*> arbitrary

deriving instance Arbitrary a => Arbitrary (GCounter a)

deriving instance (Arbitrary a, Ord a) => Arbitrary (ORSet a)

deriving instance (Arbitrary a, Ord a) => Arbitrary (LwwElementSet a)

instance Arbitrary a => Arbitrary (PNCounter a) where
    arbitrary = PNCounter <$> arbitrary <*> arbitrary

deriving instance (Ord a, Arbitrary a) => Arbitrary (Cv.TwoPSet a)

instance Arbitrary LamportTime where
    arbitrary = LamportTime <$> arbitrary <*> arbitrary

deriving instance Arbitrary Pid

-- | Generate deterministically
seeded :: Int -> Gen a -> Gen a
seeded s (MkGen g) = MkGen $ \_ n -> g (mkQCGen s) n
