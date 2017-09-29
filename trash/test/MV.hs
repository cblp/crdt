{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module MV (mv) where

import           Algebra.Lattice.Ordered (Ordered (..))
import           Data.Aeson (ToJSON (toJSON), object, (.=))
import           Data.Aeson.TH (defaultOptions, deriveToJSON)
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap as IntMap
import           Data.Semilattice (merge)
import qualified Data.Set as Set
import qualified Data.Yaml as Yaml
import           Test.QuickCheck (Arbitrary (arbitrary))
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           CRDT.Cv.MV.Internal (Item (..), MV (..), Version (..), trace')

import           Laws (cvrdtLaws)

instance ToJSON a => ToJSON (MV a) where
    toJSON (MV items) = object [".MV" .= items]

deriving instance ToJSON a => ToJSON (Ordered a)
deriving instance Arbitrary Version
deriving instance ToJSON Version

instance ToJSON a => Show (MV a) where
    show = BS.unpack . Yaml.encode

instance Arbitrary a => Arbitrary (Item a) where
    arbitrary = Item <$> arbitrary <*> arbitrary

deriving instance Arbitrary a => Arbitrary (Ordered a)

deriving instance (Arbitrary a, Ord a) => Arbitrary (MV a)

mv :: TestTree
mv = testGroup "MV"
    [ cvrdtLaws @(MV Int)
    , testProperty "idempotencySimple 0" $ idempotencySimple 0
    , testProperty "idempotencySimple 1" $ idempotencySimple 1
    ]
  where
    idempotencySimple v =
        let x :: MV Int
            x = MV $ Set.fromList
                [ Item 1 . Version $ IntMap.fromList [(2, v)]
                , Item 1 . Version $ IntMap.fromList [(2, 4)]
                ]
        in  trace' "x" x == trace' "merged" (x `merge` x)

deriveToJSON defaultOptions ''Item
