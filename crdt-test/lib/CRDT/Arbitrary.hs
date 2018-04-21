{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}

module CRDT.Arbitrary
    ( NoNul (..)
    ) where

import           Test.QuickCheck (Arbitrary (arbitrary))
import           Test.QuickCheck.Gen (Gen (MkGen))
import           Test.QuickCheck.Instances ()
import           Test.QuickCheck.Random (mkQCGen)

import           CRDT.Cv.GCounter (GCounter (..))
import           CRDT.Cv.LwwElementSet (LwwElementSet (..))
import qualified CRDT.Cv.ORSet as CvORSet
import           CRDT.Cv.PNCounter (PNCounter (..))
import qualified CRDT.Cv.RGA as CvRGA
import qualified CRDT.Cv.TwoPSet as CvTwoPSet
import           CRDT.LamportClock (LamportTime (..), Pid (..))
import           CRDT.LWW (LWW (..))

#if ENABLE_CM
import           CRDT.Arbitrary.Cm ()
#endif /* ENABLE_CM */

instance Arbitrary a => Arbitrary (LWW a) where
    arbitrary = do
        time <- arbitrary
        value <- seeded (hash time) arbitrary
        pure LWW{value, time}
      where
        hash (LamportTime t (Pid p)) = fromIntegral t * 997 + fromIntegral p

deriving instance Arbitrary a => Arbitrary (GCounter a)

deriving instance (Arbitrary a, Ord a) => Arbitrary (CvORSet.ORSet a)

deriving instance (Arbitrary a, Ord a) => Arbitrary (LwwElementSet a)

instance Arbitrary a => Arbitrary (PNCounter a) where
    arbitrary = PNCounter <$> arbitrary <*> arbitrary

deriving instance Arbitrary a => Arbitrary (CvRGA.RGA a)

deriving instance (Ord a, Arbitrary a) => Arbitrary (CvTwoPSet.TwoPSet a)

-- | Generate deterministically
seeded :: Int -> Gen a -> Gen a
seeded s (MkGen g) = MkGen $ \_ n -> g (mkQCGen s) n

newtype NoNul = NoNul String
    deriving Show

instance Arbitrary NoNul where
    arbitrary = NoNul . filter ('\NUL' /=) <$> arbitrary
