{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE TypeApplications #-}

module Cv.RGA where

import           Prelude hiding (fail)

import           Test.QuickCheck ((.&&.), (===))

import           CRDT.Cv.RGA (RgaString, edit, fromString, pack, toString,
                              unpack)
import           CRDT.LamportClock (LamportTime (LamportTime))
import           CRDT.LamportClock.Simulation (runLamportClockSim,
                                               runProcessSim)

import           Laws (cvrdtLaws)
import           Util (expectRight, fail)

prop_fromString_toString s pid = expectRight $ do
    s' <- runLamportClockSim . runProcessSim pid $ fromString s
    pure $ toString s' === s

test_Cv = cvrdtLaws @RgaString

prop_edit v1 s2 pid = expectRight . runLamportClockSim $ do
    v2 <- runProcessSim pid $ edit s2 v1
    pure $ toString v2 === s2

prop_pack_unpack rga = unpack (pack rga) == (rga :: RgaString)

prop_fromString_pack s pid = expectRight $ do
    s' <- runLamportClockSim . runProcessSim pid $ fromString s
    pure $ case pack s' of
        [(LamportTime _ pid', atoms)] -> atoms === map Just s .&&. pid' === pid
        []                            -> s === ""
        p                             -> fail $ "cannot pack " ++ show p

-- prop_edit_pack = _ TODO(cblp, 2018-02-11)
