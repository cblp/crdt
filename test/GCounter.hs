{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module GCounter where

import           Test.Tasty.QuickCheck (property, (===))

import           CRDT.Cv.GCounter (GCounter (..), increment, query)

import           Laws (cvrdtLaws)

test_Cv = cvrdtLaws @(GCounter Int) Nothing

prop_increment = property $ \(counter :: GCounter Int) i ->
    query (increment i counter) === succ (query counter)
