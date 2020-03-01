{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cv.ORSet where

import           Prelude hiding (lookup)

import           CRDT.Cv.ORSet (ORSet, add, lookup, remove)
import           CRDT.LamportClock.Simulation (runLamportClockSim,
                                               runProcessSim)
import           CRDT.Laws (cvrdtLaws)

import           Util (expectRight)

test_Cv = cvrdtLaws @(ORSet Int)

prop_add pid (x :: Char) s =
    expectRight . runLamportClockSim . runProcessSim pid $
        not . lookup x . remove x <$> add x s

-- | Difference from 'LwwElementSet' --
-- other replica can not accidentally delete x
prop_add_merge (x :: Char) pid s1 s0 =
    expectRight . runLamportClockSim . runProcessSim pid $
        lookup x . (<> s1) <$> add x s0
