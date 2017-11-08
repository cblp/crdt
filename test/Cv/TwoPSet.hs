{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cv.TwoPSet where

import           Test.Tasty.QuickCheck (property)

import           CRDT.Cv.TwoPSet (TwoPSet (..))
import           CRDT.Cv.TwoPSet as TwoPSet

import           Laws (cvrdtLaws)

-- TODO test addition after and not-after removal

prop_remove = property $ \(s :: TwoPSet Char) i ->
    not . TwoPSet.lookup i $ TwoPSet.remove i s

prop_remove_after_add = property $ \(s :: TwoPSet Char) i ->
    not . TwoPSet.lookup i . TwoPSet.remove i $ TwoPSet.add i s

test_Cv = cvrdtLaws @(TwoPSet Char) Nothing
