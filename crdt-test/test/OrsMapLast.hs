{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module OrsMapLast where

import           Data.Text (Text)
import           Test.QuickCheck (Arbitrary, discard, (===))

import           CRDT.Cv.OrsMapLast (OrsMapLast (OrsMapLast), del, get, put)
import           CRDT.LamportClock.Simulation (ObservedTime (ObservedTime),
                                               count, evalProcessSim,
                                               runLamportClockSim,
                                               runProcessSim, stamp)
import           CRDT.Laws (cvrdtLaws)

import           Util (expectRight)

deriving newtype instance
    (Arbitrary k, Ord k, Arbitrary v) => Arbitrary (OrsMapLast k v)

type TextOml = OrsMapLast Text Text

test_Cv = cvrdtLaws @TextOml

prop_get_empty k = get k (mempty :: TextOml) === Nothing

prop_put_get pid k v (oml0 :: TextOml) =
    expectRight $ runLamportClockSim $ do
        oml1 <- runProcessSim pid $ put k v oml0
        pure $ get k oml1 === Just v

prop_del_get k (oml :: TextOml) = get k (del k oml) === Nothing

prop_concurrent_put_put (oml0 :: TextOml) pid1 pid2 k v1 v2 =
    expectRight $ runLamportClockSim $ do
        -- replica 1
        (oml1, [ObservedTime{stamp = t1, count = 1}]) <-
            evalProcessSim pid1 $ put k v1 oml0
        -- replica 2
        (oml2, [ObservedTime{stamp = t2, count = 1}]) <-
            evalProcessSim pid2 $ put k v2 oml0
        -- replica 3
        let oml3 = oml1 <> oml2
        pure $ if
            | t1 < t2   -> get k oml3 === Just v2
            | t2 < t1   -> get k oml3 === Just v1
            | otherwise -> discard

prop_concurrent_put_del (oml0 :: TextOml) pid1 k v =
    expectRight $ runLamportClockSim $ do
        -- replica 1
        oml1 <- runProcessSim pid1 $ put k v oml0
        -- replica 2
        let oml2 = del k oml0
        -- replica 3
        let oml3 = oml1 <> oml2
        pure $ get k oml3 === Just v
