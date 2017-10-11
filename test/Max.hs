{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Max where

import           Test.Tasty.QuickCheck (property, (===))

import           CRDT.Cv.Max (Max, point, query)
import           Data.Semilattice (merge)

import           Laws (cvrdtLaws)

test_Cv = cvrdtLaws @(Max Int)

prop_merge = property $ \(m :: Max Int) i ->
    query (point i `merge` m) === max i (query m)
