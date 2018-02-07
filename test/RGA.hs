{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module RGA where

import           Control.Monad.State.Strict (get)
import           Data.Maybe (isJust)
import qualified Data.Vector as Vector
import           Test.QuickCheck (conjoin, counterexample, (===))

import           CRDT.Cm (apply, initial, makeAndApplyOp, makeOp)
import           CRDT.Cm.RGA (RGA (OpAddAfter), RgaIntent (AddAfter),
                              fromString, load, toString)
import           CRDT.LamportClock (LamportTime (LamportTime), Pid (Pid),
                                    advance)
import           CRDT.LamportClock.Simulation (ProcessSim, runLamportClockSim,
                                               runProcessSim)

import           Laws (cmrdtLaw)
import           Util (pattern (:-), expectRightK)

prop_makeOp =
    isJust
        (makeOp @(RGA Char) (AddAfter Nothing 'a') (initial @(RGA Char))
        :: Maybe (ProcessSim () (RGA Char)))

prop_makeAndApplyOp = conjoin
    [ counterexample "result3"  $ result3  === Right (op3, payload3)
    , counterexample "result2"  $ result2  === Right (op2, payload2)
    , counterexample "result1"  $ result1  === Right (op1, payload1)
    , counterexample "result12" $ result12 === payload12
    , counterexample "results=" $ result21 === result12
    ]
  where
    time1 = LamportTime 4 $ Pid 1
    time2 = LamportTime 4 $ Pid 2
    time3 = LamportTime 3 $ Pid 3
    op1 = OpAddAfter Nothing '1' time1
    op2 = OpAddAfter Nothing '2' time2
    op3 = OpAddAfter Nothing '3' time3
    payload3 = load $ Vector.singleton (time3 :- Just '3')
    payload1 = load $ Vector.fromList [time1 :- Just '1', time3 :- Just '3']
    payload2 = load $ Vector.fromList [time2 :- Just '2', time3 :- Just '3']
    payload12 = load $ Vector.fromList
        [time2 :- Just '2', time1 :- Just '1', time3 :- Just '3']
    result3 = runLamportClockSim (initial @(RGA Char)) $
        runProcessSim (Pid 3) $ do
            advance 2
            (,) <$> makeAndApplyOp @(RGA Char) (AddAfter Nothing '3')
                <*> get
    result2 = runLamportClockSim payload3 $ runProcessSim (Pid 2) $ do
        advance 1
        (,) <$> makeAndApplyOp @(RGA Char) (AddAfter Nothing '2')
            <*> get
    result1 = runLamportClockSim payload3 $ runProcessSim (Pid 1) $
        (,) <$> makeAndApplyOp @(RGA Char) (AddAfter Nothing '1')
            <*> get
    result12 = apply op2 payload1
    result21 = apply op1 payload2

prop_fromString s pid =
    result ===
    Right (load $ Vector.fromList
        [LamportTime t pid :- Just c | t <- [1..] | c <- s])
  where
    result = runLamportClockSim (initial @(RGA Char)) $
        runProcessSim pid $ do
            _ <- fromString s
            get

prop_fromString_toString s pid =
    expectRightK result $ \s' -> toString s' === s
  where
    result = runLamportClockSim (initial @(RGA Char)) $ runProcessSim pid $ do
        _ <- fromString s
        get

prop_Cm = cmrdtLaw @(RGA Char)
