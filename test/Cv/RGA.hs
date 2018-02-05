{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE TypeApplications #-}

module Cv.RGA where

import           Test.QuickCheck ((===))

import           CRDT.Cv.RGA (RgaString, edit, fromString, toString)
import           CRDT.LamportClock.Simulation (runLamportClockSim,
                                               runProcessSim)

import           Laws (cvrdtLaws)
import           Util (expectRight)

prop_fromString_toString s pid = expectRight $ do
    s' <- runLamportClockSim undefined $ runProcessSim pid $ fromString s
    pure $ toString s' === s

test_Cv = cvrdtLaws @RgaString

prop_edit v1 s2 pid = expectRight . runLamportClockSim undefined $ do
    v2 <- runProcessSim pid $ edit s2 v1
    pure $ toString v2 === s2
