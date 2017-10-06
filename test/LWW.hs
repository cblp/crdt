{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module LWW (lww) where

import           Data.Semigroup ((<>))
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           CRDT.LWW (LWW (..), assign, initial, query)
import           LamportClock (barrier, runLamportClock, runProcess)

import           Laws (cmrdtLaw, cvrdtLaws)

lww :: TestTree
lww = testGroup "LWW"
    [ cmrdtLaw @(LWW Int)
    , testGroup "Cv"
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
    ]
