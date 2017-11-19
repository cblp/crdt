{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ORSet where

import           Prelude hiding (lookup)

import           Data.Semigroup (Semigroup ((<>)))

import           CRDT.Cv.ORSet (ORSet (..), add, lookup, remove)

import           Laws (cvrdtLaws)

test_Cv = cvrdtLaws @(ORSet Int) Nothing

prop_add pid (x :: Char) = not . lookup x . remove x . add pid x

-- | Difference from 'LwwElementSet' --
-- other replica can not accidentally delete x
prop_add_merge s (x :: Char) pid = lookup x . (<> s) . add pid x
