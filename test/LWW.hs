{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module LWW
    ( prop_Cm
    , prop_assign
    , prop_merge_with_former
    , test_Cv
    ) where

import           Data.Semigroup ((<>))
import           Test.QuickCheck ((===))

import           CRDT.LamportClock (runLamportClockSim, runProcessSim)
import           CRDT.LWW (LWW, assign, initial, query)

import           Laws (cmrdtLaw, cvrdtLaws)

prop_Cm = cmrdtLaw @(LWW Char)

test_Cv = cvrdtLaws @(LWW Char)

prop_assign pid1 pid2 (formerValue :: Char) latterValue =
    runLamportClockSim $ do
        state1 <- runProcessSim pid1 $ initial formerValue
        state2 <- runProcessSim pid2 $ assign latterValue state1
        pure $ query state2 === latterValue

prop_merge_with_former pid1 pid2 (formerValue :: Char) latterValue =
    runLamportClockSim $ do
        state1 <- runProcessSim pid1 $ initial formerValue
        state2 <- runProcessSim pid2 $ assign latterValue state1
        pure $ query (state1 <> state2) === latterValue
