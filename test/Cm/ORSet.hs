{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cm.ORSet where

import qualified Data.MultiMap as MultiMap
import           Test.QuickCheck (counterexample, (.&&.), (===), (==>))

import           CRDT.Cm (initial, makeAndApplyOp, query)
import           CRDT.Cm.ORSet (Intent (Add, Remove), ORSet, Tag (Tag),
                                elements)
import           CRDT.LamportClock.Simulation (runLamportClockSim,
                                               runProcessSim)

import           Laws (cmrdtLaw)
import           Util (pattern (:-), expectRight)

prop_Cm = cmrdtLaw @(ORSet Char)

-- | Example from fig. 14 from "A comprehensive study of CRDTs"
prop_fig14 α β a = expectRight . runLamportClockSim (initial @(ORSet Char)) $ do
    op1 <- runProcessSim β $ makeAndApplyOp (Add (a :: Char))
    op2 <- runProcessSim α $ makeAndApplyOp (Add a)
    op3 <- runProcessSim α $ makeAndApplyOp (Remove a)
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
query' = MultiMap.assocs . elements . query
