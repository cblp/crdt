{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module MV (mv) where

import           Algebra.Lattice.Ordered (Ordered (..))
import           Test.QuickCheck (Arbitrary)
import           Test.Tasty (TestTree, testGroup)

import           CRDT.Cv.MV.Internal (MV (..))

import           Laws (cvrdtLaws)

deriving instance Arbitrary a => Arbitrary (Ordered a)

deriving instance (Arbitrary a, Ord a) => Arbitrary (MV a)

mv :: TestTree
mv = testGroup "MV" [cvrdtLaws @(MV Int)]
