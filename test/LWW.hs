{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module LWW (lww) where

import           Control.Monad.Reader (runReader)
import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (Positive (..), testProperty)

import           CRDT.Cv.LWW (LWW, assign, initial, query)
import           CRDT.Timestamp (Timestamp)

import           Laws (cvrdtLaws)

initial' :: Timestamp -> a -> LWW a
initial' t v = runReader (initial v) t

assign' :: Ord a => Timestamp -> a -> LWW a -> LWW a
assign' t v s = runReader (assign v s) t

instance Arbitrary a => Arbitrary (LWW a) where
    arbitrary = initial' <$> arbitrary <*> arbitrary

lww :: TestTree
lww = testGroup "LWW"
    [ cvrdtLaws @(LWW Int)
    , testProperty "assign latter" $
        \formerTime (formerValue :: Int) (Positive dt) latterValue -> let
            latterTime = formerTime + dt
            state = initial' formerTime formerValue
            state' = assign' latterTime latterValue state
            in
            query state' == latterValue
    , testProperty "assign former" $
        \formerTime (formerValue :: Int) (Positive dt) latterValue -> let
            latterTime = formerTime + dt
            state = initial' latterTime latterValue
            state' = assign' formerTime formerValue state
            in
            query state' == latterValue
    ]
