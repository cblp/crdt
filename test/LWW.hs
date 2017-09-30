{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module LWW (lww) where

import           Data.Semigroup ((<>))
import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           CRDT.Cv.LWW (LWW (..), assign, initial, query)
import           LamportClock (Timestamp (..), runLamportClock, runProcess)
import           LamportClock.Internal (Pid (..), barrier)

import           Laws (cvrdtLaws)

deriving instance Arbitrary Pid

instance Arbitrary Timestamp where
    arbitrary = Timestamp <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (LWW a) where
    arbitrary = LWW <$> arbitrary <*> arbitrary

lww :: TestTree
lww = testGroup "LWW"
    [ cvrdtLaws @(LWW Int)
    , testProperty "assign" $
        \pid1 pid2 (formerValue :: Int) latterValue ->
            runLamportClock $ do
                state1 <- runProcess pid1 $ initial formerValue
                barrier [pid1, pid2]
                state2 <- runProcess pid2 $ assign latterValue state1
                pure $ query state2 == latterValue
    , testProperty "merge with former" $
        \pid1 pid2 (formerValue :: Int) latterValue ->
            runLamportClock $ do
                state1 <- runProcess pid1 $ initial formerValue
                barrier [pid1, pid2]
                state2 <- runProcess pid2 $ initial latterValue
                pure $ query (state1 <> state2) == latterValue
    ]
