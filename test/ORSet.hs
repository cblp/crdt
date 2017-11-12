{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ORSet where

import           Test.Tasty.QuickCheck (property)

import           CRDT.Cv.ORSet (ORSet (..))
import qualified CRDT.Cv.ORSet as ORSet
import           Data.Semigroup (Semigroup ((<>)))

import           Laws (cvrdtLaws)

test_Cv = cvrdtLaws @(ORSet Int) Nothing

prop_insert = property $ \pid i ->
    not . ORSet.lookup i $ ORSet.remove pid i (ORSet.add pid i (ORSet.initial :: ORSet Int))

prop_add_merge = property $ \(s0 :: ORSet Int) s2 x pid ->
    let s1 = ORSet.add pid x s0; in ORSet.lookup x (s1 <> s2)