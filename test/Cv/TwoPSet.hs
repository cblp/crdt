{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cv.TwoPSet where

import           Prelude hiding (lookup)

import           Test.QuickCheck ((==>))

import           CRDT.Cv.TwoPSet (TwoPSet (..), add, isKnown, lookup, remove)

import           Laws (cvrdtLaws)

-- TODO test addition after and not-after removal

prop_add_first (s :: TwoPSet Char) x = not (isKnown x s) ==> lookup x (add x s)

prop_remove (s :: TwoPSet Char) x = not . lookup x $ remove x s

prop_remove_after_add (s :: TwoPSet Char) x =
    not . lookup x . remove x $ add x s

test_Cv = cvrdtLaws @(TwoPSet Char) Nothing
