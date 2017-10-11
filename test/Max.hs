{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Max where

import           Test.Tasty.QuickCheck (property, (===))

import           CRDT.Cv.Max (Max, initial, query)
import           Data.Semilattice (merge)

import           Laws (cvrdtLaws)

test_Cv = cvrdtLaws @(Max Int)

prop_merge = property $ \(x :: Int) y ->
    query (initial x `merge` initial y) === max x y
