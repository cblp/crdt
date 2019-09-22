{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

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

import           CRDT.LamportClock.Simulation (runLamportClockSim,
                                               runProcessSim)
import           CRDT.Laws (cmrdtLaw, cvrdtLaws)
import           CRDT.LWW (LWW, assign, initialize, query)

import           Util (expectRight)

prop_Cm = cmrdtLaw @(LWW Char)

test_Cv = cvrdtLaws @(LWW Char)

prop_assign pid1 pid2 (formerValue :: Char) latterValue =
    expectRight . runLamportClockSim $ do
        state1 <- runProcessSim pid1 $ initialize formerValue
        state2 <- runProcessSim pid2 $ assign latterValue state1
        pure $ query state2 === latterValue

prop_merge_with_former pid1 pid2 (formerValue :: Char) latterValue =
    expectRight . runLamportClockSim $ do
        state1 <- runProcessSim pid1 $ initialize formerValue
        state2 <- runProcessSim pid2 $ assign latterValue state1
        pure $ query (state1 <> state2) === latterValue
