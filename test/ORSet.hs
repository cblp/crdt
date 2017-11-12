{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ORSet where

import           Test.Tasty.QuickCheck (property)

import           CRDT.Cv.ORSet (ORSet (..))
import qualified CRDT.Cv.ORSet as ORSet
import           LamportClock (Pid(..))

import           Laws (cvrdtLaws)

test_Cv = cvrdtLaws @(ORSet Int)

prop_insert = property $ \(pid :: Pid) i ->
    not . ORSet.lookup i $ ORSet.remove pid i (ORSet.add pid i (ORSet.initial :: ORSet Int))
