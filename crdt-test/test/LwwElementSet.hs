{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module LwwElementSet
    ( prop_add
    , prop_no_removal_bias
    , prop_remove
    , prop_they_accidentally_delete_our_value
    , test_Cv
    ) where

import           Prelude hiding (lookup)

import           CRDT.Cv.LwwElementSet (LwwElementSet, add, lookup, remove)
import           CRDT.LamportClock (LamportTime (LamportTime), advance, getTime)
import           CRDT.LamportClock.Simulation (runLamportClockSim,
                                               runProcessSim)
import           CRDT.Laws (cvrdtLaws)

import           Util (expectRight)

test_Cv = cvrdtLaws @(LwwElementSet Char)

prop_add (s :: LwwElementSet Char) x pid1 =
    expectRight . runLamportClockSim $ do
        s1 <- runProcessSim pid1 $ add x s
        pure $ lookup x s1

prop_remove (s :: LwwElementSet Char) x pid1 pid2 =
    expectRight . runLamportClockSim $ do
        s1 <- runProcessSim pid1 $ add x s
        s2 <- runProcessSim pid2 $ remove x s1
        pure . not $ lookup x s2

-- | Difference from 'TwoPSet' -- no removal bias
prop_no_removal_bias (s :: LwwElementSet Char) x pid1 pid2 pid3 =
    expectRight . runLamportClockSim $ do
        s1 <- runProcessSim pid1 $ add x s
        s2 <- runProcessSim pid2 $ remove x s1
        s3 <- runProcessSim pid3 $ add x s2
        pure $ lookup x s3

-- | Difference from 'ORSet' -- other replica can accidentally delete x
prop_they_accidentally_delete_our_value (s :: LwwElementSet Char) x pid1 pid2 =
    expectRight . runLamportClockSim $ do
        (s1, LamportTime t0 _) <-
            runProcessSim pid1 $ (,) <$> add x s <*> getTime
        s2 <- runProcessSim pid2 $ do
            advance t0
            s' <- add x s
            remove x s'
        pure . not . lookup x $ s1 <> s2
