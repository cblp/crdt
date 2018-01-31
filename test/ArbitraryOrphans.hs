{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}

module ArbitraryOrphans () where

import           Test.QuickCheck (Arbitrary (..), arbitraryBoundedEnum,
                                  elements, oneof)
import           Test.QuickCheck.Gen (Gen (MkGen))
import           Test.QuickCheck.Instances ()
import           Test.QuickCheck.Random (mkQCGen)

import           CRDT.Cm.Counter (Counter (..))
import           CRDT.Cm.GSet (GSet (..))
import           CRDT.Cm.ORSet (MultiMap (..))
import qualified CRDT.Cm.ORSet as CmORSet
import qualified CRDT.Cm.TwoPSet as CmTwoPSet
import           CRDT.Cv.GCounter (GCounter (..))
import           CRDT.Cv.LwwElementSet (LwwElementSet (..))
import qualified CRDT.Cv.ORSet as CvORSet
import           CRDT.Cv.PNCounter (PNCounter (..))
import qualified CRDT.Cv.TwoPSet as CvTwoPSet
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

deriving instance
        (Arbitrary k, Ord k, Arbitrary v, Ord v) => Arbitrary (MultiMap k v)

instance Arbitrary a => Arbitrary (CmORSet.Intent a) where
    arbitrary = elements [CmORSet.Add, CmORSet.Remove] <*> arbitrary

instance Arbitrary a => Arbitrary (CmORSet.ORSet a) where
    arbitrary = oneof
        [ CmORSet.OpAdd    <$> arbitrary <*> arbitrary
        , CmORSet.OpRemove <$> arbitrary <*> arbitrary
        ]

instance (Arbitrary a, Ord a) => Arbitrary (CmORSet.Payload a) where
    arbitrary = CmORSet.Payload <$> arbitrary <*> arbitrary

instance Arbitrary CmORSet.Tag where
    arbitrary = CmORSet.Tag <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (CmTwoPSet.TwoPSet a) where
    arbitrary = elements [CmTwoPSet.Add, CmTwoPSet.Remove] <*> arbitrary

deriving instance Arbitrary a => Arbitrary (GCounter a)

deriving instance (Arbitrary a, Ord a) => Arbitrary (CvORSet.ORSet a)

deriving instance (Arbitrary a, Ord a) => Arbitrary (LwwElementSet a)

instance Arbitrary a => Arbitrary (PNCounter a) where
    arbitrary = PNCounter <$> arbitrary <*> arbitrary

deriving instance (Ord a, Arbitrary a) => Arbitrary (CvTwoPSet.TwoPSet a)

instance Arbitrary LamportTime where
    arbitrary = LamportTime <$> arbitrary <*> arbitrary

deriving instance Arbitrary Pid

-- | Generate deterministically
seeded :: Int -> Gen a -> Gen a
seeded s (MkGen g) = MkGen $ \_ n -> g (mkQCGen s) n
