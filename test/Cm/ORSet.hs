{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cm.ORSet where

import           Control.Monad.State.Strict (MonadState, get, modify)
import           Data.Maybe (fromJust)
import           Test.QuickCheck (counterexample, (.&&.), (===), (==>))

import           CRDT.Cm (apply, makeOp)
import qualified CRDT.Cm as Cm
import           CRDT.Cm.ORSet (Intent (Add, Remove), ORSet, Payload, Tag (Tag),
                                elements, initial, multiMapAssocs)
import           CRDT.LamportClock (Clock)
import           CRDT.LamportClock.Simulation (runLamportClockSim,
                                               runProcessSim)

import           Laws (cmrdtLaw)

prop_Cm = cmrdtLaw @(ORSet Char)

-- | Example from fig. 14 from "A comprehensive study of CRDTs"
prop_fig14 α β a = runLamportClockSim initial $ do
    op1 <- runProcessSim β $ atSource' $ Add (a :: Char)
    op2 <- runProcessSim α $ atSource' $ Add a
    op3 <- runProcessSim α $ atSource' $ Remove a
    pure $
        α < β ==>
        check "2"   [op2]           [a :- [Tag α 0]]          .&&.
        check "23"  [op2, op3]      []                        .&&.
        check "231" [op2, op3, op1] [a :- [Tag β 0]]          .&&.
        check "1"   [op1]           [a :- [Tag β 0]]          .&&.
        check "12"  [op1, op2]      [a :- [Tag α 0, Tag β 0]] .&&.
        check "123" [op1, op2, op3] [a :- [Tag β 0]]
  where
    check opsLabel ops result =
        counterexample ("ops = " ++ opsLabel) $
        counterexample ("ops = " ++ show ops) $
        query' ops === result

query' :: (Ord a, Foldable f) => f (ORSet a) -> [(a, [Tag])]
query' = multiMapAssocs . elements . Cm.query initial

atSource'
    :: (Clock m, MonadState (Payload a) m, Ord a) => Intent a -> m (ORSet a)
atSource' intent = do
    payload <- get
    op <- fromJust $ makeOp intent payload
    modify $ apply op
    pure op

pattern (:-) :: a -> b -> (a, b)
pattern a :- b = (a, b)
