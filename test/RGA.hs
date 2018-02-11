{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module RGA where

import           Prelude hiding (fail)

import           Control.Monad.State.Strict (execStateT, runStateT)
import           Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import           Data.Maybe (isJust)
import qualified Data.Vector as Vector
import           Test.QuickCheck (Property, conjoin, counterexample, property,
                                  (.&&.), (===))

import           CRDT.Cm (apply, initial, makeAndApplyOp, makeOp)
import           CRDT.Cm.RGA (RGA (OpAddAfter, OpRemove), RgaIntent (AddAfter),
                              RgaPayload (RgaPayload), fromString, load,
                              toString)
import           CRDT.LamportClock (LamportTime (LamportTime), Pid (Pid),
                                    advance)
import           CRDT.LamportClock.Simulation (ProcessSim, runLamportClockSim,
                                               runProcessSim)

import           Laws (cmrdtLaw)
import           Util (pattern (:-), expectRightK)

prop_makeOp = isJust $ makeOp @(RGA Char) @ProcessSim
    (AddAfter Nothing 'a')
    (initial @(RGA Char))

prop_makeAndApplyOp = conjoin
    [ counterexample "result3" $ expectRightK result3 $ \(op, payload) ->
        counterexample ("op = " ++ show op) (opsEqWoTime op op3)
            .&&. counterexample "payload" (payloadsEqWoTime payload payload3)
    , counterexample "result2" $ expectRightK result2 $ \(op, payload) ->
        counterexample ("op = " ++ show op) (opsEqWoTime op op2)
            .&&. counterexample "payload" (payloadsEqWoTime payload payload2)
    , counterexample "result1" $ expectRightK result1 $ \(op, payload) ->
        counterexample ("op = " ++ show op) (opsEqWoTime op op1)
            .&&. counterexample "payload" (payloadsEqWoTime payload payload1)
    , counterexample "result12" $ result12 === payload12
    , counterexample "results=" $ result21 === result12
    ]
  where
    time1     = LamportTime 4 $ Pid 1 -- TODO(cblp, 2018-02-11) arbitrary pids
    time2     = LamportTime 4 $ Pid 2
    time3     = LamportTime 3 $ Pid 3
    op1       = OpAddAfter Nothing '1' time1
    op2       = OpAddAfter Nothing '2' time2
    op3       = OpAddAfter Nothing '3' time3
    payload3  = load $ Vector.singleton (time3 :- Just '3')
    payload1  = load $ Vector.fromList [time1 :- Just '1', time3 :- Just '3']
    payload2  = load $ Vector.fromList [time2 :- Just '2', time3 :- Just '3']
    payload12 = load $ Vector.fromList
        [time2 :- Just '2', time1 :- Just '1', time3 :- Just '3']
    result3 =
        runLamportClockSim
            . runProcessSim (Pid 3)
            . (`runStateT` initial @(RGA Char))
            $ do
                  advance 2
                  makeAndApplyOp @(RGA Char) (AddAfter Nothing '3')
    result2 =
        runLamportClockSim . runProcessSim (Pid 2) . (`runStateT` payload3) $ do
            advance 1
            makeAndApplyOp @(RGA Char) (AddAfter Nothing '2')
    result1 =
        runLamportClockSim
            . runProcessSim (Pid 1)
            . (`runStateT` payload3)
            $ makeAndApplyOp @(RGA Char) (AddAfter Nothing '1')
    result12 = apply op2 payload1
    result21 = apply op1 payload2

prop_fromString s pid =
    expectRightK result $ payloadsEqWoTime $ load $ Vector.fromList
        [ LamportTime t pid :- Just c | t <- [1..] | c <- s ]
  where
    result =
        runLamportClockSim
            . runProcessSim pid
            . (`execStateT` initial @(RGA Char))
            $ fromString s

prop_fromString_toString s pid = expectRightK result $ \s' -> toString s' === s
  where
    result =
        runLamportClockSim
            . runProcessSim pid
            . (`execStateT` initial @(RGA Char))
            $ fromString s

prop_Cm = cmrdtLaw @(RGA Char)

ok = property ()

fail s = counterexample s $ property False

-- | Ops equal without local times
opsEqWoTime (OpAddAfter parent1 atom1 id1) (OpAddAfter parent2 atom2 id2) =
    conjoin
        [ counterexample "parent" $ pidsMaybeEq parent1 parent2
        , counterexample "atom" $ atom1 === atom2
        , counterexample "id" $ pidsEqWoTime id1 id2
        ]
opsEqWoTime (OpRemove parent1) (OpRemove parent2) =
    counterexample "parent" $ pidsEqWoTime parent1 parent2
opsEqWoTime x y = fail $ show x ++ " /= " ++ show y

pidsEqWoTime (LamportTime _ pid1) (LamportTime _ pid2) = pid1 === pid2

pidsMaybeEq Nothing  Nothing  = ok
pidsMaybeEq (Just x) (Just y) = pidsEqWoTime x y
pidsMaybeEq x        y        = fail $ show x ++ " /= " ++ show y

payloadsEqWoTime :: (Eq a, Show a) => RgaPayload a -> RgaPayload a -> Property
payloadsEqWoTime (RgaPayload vertices1 vertexIxs1) (RgaPayload vertices2 vertexIxs2)
    = conjoin
        [ counterexample "vertices" $ conjoin
            [ counterexample ("[" ++ show i ++ "]")
              $    counterexample "id"   (pidsEqWoTime id1 id2)
              .&&. counterexample "atom" (a1 === a2)
            | i <- [0 :: Int ..] | (id1, a1) <- toList vertices1 | (id2, a2) <- toList vertices2
            ]
        , counterexample "vertexIxs" $ conjoin
            [ counterexample "id" (pidsEqWoTime id1 id2)
                  .&&. counterexample "ix" (ix1 === ix2)
            | (id1, ix1) <- Map.assocs vertexIxs1 | (id2, ix2) <- Map.assocs vertexIxs2
            ]
        ]
