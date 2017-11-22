{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ORSet where

import           Prelude hiding (lookup)

import           Data.Semigroup (Semigroup ((<>)))

import           CRDT.Cv.ORSet (ORSet (..), add, lookup, remove)
import           CRDT.LamportClock (runLamportClock, runProcess)

import           Laws (cvrdtLaws)

test_Cv = cvrdtLaws @(ORSet Int) Nothing

prop_add pid (x :: Char) s =
    runLamportClock $ runProcess pid $ not . lookup x . remove x <$> add x s

-- | Difference from 'LwwElementSet' --
-- other replica can not accidentally delete x
prop_add_merge (x :: Char) pid s1 s0 =
    runLamportClock $ runProcess pid $ lookup x . (<> s1) <$> add x s0
