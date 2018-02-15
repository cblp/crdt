{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module CRDT.Arbitrary.Cm () where

import           Test.QuickCheck (Arbitrary (..), arbitraryBoundedEnum,
                                  elements, frequency, oneof)

import           CRDT.Cm.Counter (Counter (..))
import           CRDT.Cm.GSet (GSet (..))
import qualified CRDT.Cm.ORSet as CmORSet
import qualified CRDT.Cm.RGA as CmRGA
import qualified CRDT.Cm.TwoPSet as CmTwoPSet

import           CRDT.Arbitrary.Common ()

instance Arbitrary (Counter a) where
    arbitrary = arbitraryBoundedEnum

deriving instance Arbitrary a => Arbitrary (GSet a)

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

instance Arbitrary a => Arbitrary (CmRGA.RGA a) where
    arbitrary = oneof
        [ CmRGA.OpAddAfter <$> arbitrary <*> arbitrary <*> arbitrary
        , CmRGA.OpRemove   <$> arbitrary
        ]

instance Arbitrary a => Arbitrary (CmRGA.RgaIntent a) where
    arbitrary = frequency
        [ (10, CmRGA.AddAfter <$> arbitrary <*> arbitrary)
        , ( 1, CmRGA.Remove   <$> arbitrary)
        ]

instance (Arbitrary a, Ord a) => Arbitrary (CmRGA.RgaPayload a) where
    arbitrary = CmRGA.load <$> arbitrary

instance Arbitrary a => Arbitrary (CmTwoPSet.TwoPSet a) where
    arbitrary = elements [CmTwoPSet.Add, CmTwoPSet.Remove] <*> arbitrary
