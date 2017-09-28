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

import           CRDT (runPidClock, runProcess)
import           CRDT.Cv.LWW (assign, initial, query)
import           CRDT.Cv.LWW.Internal (LWW (..))
import           CRDT.Internal (Pid (..), barrier)

import           Laws (cvrdtLaws)

deriving instance Arbitrary Pid

instance Arbitrary a => Arbitrary (LWW a) where
    arbitrary = initial' <$> arbitrary <*> arbitrary where
        initial' p v = runPidClock . runProcess p $ initial v

lww :: TestTree
lww = testGroup "LWW"
    [ cvrdtLaws @(LWW Int)
    , testProperty "assign" $
        \pid1 pid2 (formerValue :: Int) latterValue ->
            runPidClock $ do
                state1 <- runProcess pid1 $ initial formerValue
                barrier [pid1, pid2]
                state2 <- runProcess pid2 $ assign latterValue state1
                pure $ query state2 == latterValue
    , testProperty "merge with former" $
        \pid1 pid2 (formerValue :: Int) latterValue ->
            runPidClock $ do
                state1 <- runProcess pid1 $ initial formerValue
                barrier [pid1, pid2]
                state2 <- runProcess pid2 $ initial latterValue
                pure $ query (state1 <> state2) == latterValue
    ]
