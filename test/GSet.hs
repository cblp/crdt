{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module GSet where

import           Test.Tasty.QuickCheck (property)

import qualified CRDT.Cm.GSet as Cm
import qualified CRDT.Cv.GSet as Cv

import           Laws (cmrdtLaw, cvrdtLaws)

prop_Cm = cmrdtLaw @(Cm.GSet Char) Nothing

test_Cv = cvrdtLaws @(Cv.GSet Char) Nothing

prop_add = property $ \(set :: Cv.GSet Char) i -> Cv.lookup i (Cv.add i set)
