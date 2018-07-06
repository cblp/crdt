{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cv.TwoPSet where

import           Test.QuickCheck ((==>))

import           CRDT.Cv.TwoPSet (TwoPSet (..), add, isKnown, member, remove)
import           CRDT.Laws (cvrdtLaws)

-- | Difference from LwwElementSet and ORSet -- removal bias
prop_removal_bias (s :: TwoPSet Char) x =
    not . member x . add x . remove x $ add x s

prop_add (s :: TwoPSet Char) x = not (isKnown x s) ==> member x (add x s)

prop_remove (s :: TwoPSet Char) x = not . member x $ remove x s

prop_add_then_remove (s :: TwoPSet Char) x =
    not . member x . remove x $ add x s

test_Cv = cvrdtLaws @(TwoPSet Char)
