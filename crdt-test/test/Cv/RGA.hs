{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE TypeApplications #-}

module Cv.RGA where

import           Prelude hiding (fail)

import           Test.QuickCheck (conjoin, (.&&.), (.||.), (===))

import           CRDT.Arbitrary (NoNul (..))
import           CRDT.Cv.RGA (RgaString, edit, fromString, pack, toString,
                              unpack)
import           CRDT.LamportClock (LamportTime (LamportTime))
import           CRDT.LamportClock.Simulation (runLamportClockSim,
                                               runProcessSim)
import           CRDT.Laws (cvrdtLaws)

import           Util (expectRight, fail)

prop_fromString_toString (NoNul s) pid = expectRight $ do
    v <- runLamportClockSim . runProcessSim pid $ fromString s
    pure $ toString v === s

test_Cv = cvrdtLaws @RgaString

prop_edit v1 (NoNul s2) pid = expectRight . runLamportClockSim $ do
    v2 <- runProcessSim pid $ edit s2 v1
    pure $ toString v2 === s2

prop_pack_unpack rga = unpack (pack rga) == (rga :: RgaString)

prop_fromString_pack s pid = expectRight $ do
    v <- runLamportClockSim . runProcessSim pid $ fromString s
    pure $ case pack v of
        [(LamportTime _ pid', atoms)] -> atoms === s .&&. pid' === pid
        []                            -> s === ""
        p                             -> fail $ "cannot pack " ++ show p

prop_edit_pack s1 s2 pid1 pid2 = expectRight . runLamportClockSim $ do
    v1 <- runProcessSim pid1 $ fromString s1
    v2 <- runProcessSim pid2 $ edit (s1 ++ s2) v1
    pure $ case pack v2 of
        [(LamportTime _ pid1', atoms1), (LamportTime _ pid2', atoms2)] ->
            conjoin
                [atoms1 === s1, pid1' === pid1, atoms2 === s2, pid2' === pid2]
        [(LamportTime _ pid', atoms)] ->
            (atoms === s1 .&&. pid' === pid1 .&&. s2 === "")
                .||. (atoms === s2 .&&. pid' === pid2 .&&. s1 === "")
        [] -> s1 === "" .&&. s2 === ""
        p  -> fail $ "cannot pack " ++ show p
