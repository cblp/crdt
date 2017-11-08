{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE TypeApplications #-}

module Cm.TwoPSet where

import           CRDT.Cm.TwoPSet (TwoPSet)

import           Laws (cmrdtLaw)

prop_Cm = cmrdtLaw @(TwoPSet Char) Nothing
