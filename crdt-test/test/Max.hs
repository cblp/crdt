{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Max where

import           Test.QuickCheck ((===))

import           CRDT.Cv.Max (Max, initial, query)
import           CRDT.Laws (cvrdtLaws)
import           Data.Semilattice (merge)

test_Cv = cvrdtLaws @(Max Char)

prop_merge (x :: Char) y = query (initial x `merge` initial y) === max x y
