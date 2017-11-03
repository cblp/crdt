{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module LWW where

import           Data.Semigroup ((<>))
import           Test.Tasty.QuickCheck (property, (===))

import           CRDT.LWW (LWW (..), assignP, initialP, query)
import           LamportClock (barrier, runLamportClock, runProcess)

import           Laws (cmrdtLaw, cvrdtLaws)

prop_Cm = cmrdtLaw @(LWW Int)

test_Cv = cvrdtLaws @(LWW Int)

prop_assign = property $ \pid1 pid2 (formerValue :: Int) latterValue ->
    runLamportClock $ do
        state1 <- runProcess pid1 $ initialP formerValue
        barrier [pid1, pid2]
        state2 <- runProcess pid2 $ assignP latterValue state1
        pure $ query state2 === latterValue

prop_merge_with_former = property $
    \pid1 pid2 (formerValue :: Int) latterValue -> runLamportClock $ do
        state1 <- runProcess pid1 $ initialP formerValue
        barrier [pid1, pid2]
        state2 <- runProcess pid2 $ initialP latterValue
        pure $ query (state1 <> state2) === latterValue
