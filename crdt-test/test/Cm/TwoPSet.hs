{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE TypeApplications #-}

module Cm.TwoPSet where

import           Test.QuickCheck (Small)

import           CRDT.Cm.TwoPSet (TwoPSet (Remove))
import           CRDT.Laws (cmrdtLaw, opCommutativity)

prop_Cm = cmrdtLaw @(TwoPSet Char)

prop_remove_commutes_with_itself e = opCommutativity intentOp intentOp
  where
    intent = Remove (e :: Small Int)
    op = intent
    intentOp = (intent, op)
