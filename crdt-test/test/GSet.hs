{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module GSet where

import qualified CRDT.Cm.GSet as Cm
import qualified CRDT.Cv.GSet as Cv

import           Laws (cmrdtLaw, cvrdtLaws)

prop_Cm = cmrdtLaw @(Cm.GSet Char)

test_Cv = cvrdtLaws @(Cv.GSet Char)

prop_add (x :: Char) = Cv.lookup x . Cv.add x
