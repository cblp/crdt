{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cv.TwoPSet where

import           Prelude hiding (lookup)

import           Test.QuickCheck ((==>))

import           CRDT.Cv.TwoPSet (TwoPSet (..), add, isKnown, lookup, remove)

import           Laws (cvrdtLaws)

-- | Difference from LwwElementSet and ORSet -- removal bias
prop_removal_bias (s :: TwoPSet Char) x =
    not . lookup x . add x . remove x $ add x s

prop_add (s :: TwoPSet Char) x = not (isKnown x s) ==> lookup x (add x s)

prop_remove (s :: TwoPSet Char) x = not . lookup x $ remove x s

prop_add_then_remove (s :: TwoPSet Char) x =
    not . lookup x . remove x $ add x s

test_Cv = cvrdtLaws @(TwoPSet Char) Nothing
