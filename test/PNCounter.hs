{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module PNCounter where

import           Test.Tasty.QuickCheck (property, (===))

import           CRDT.Cv.PNCounter (PNCounter (..), decrement, increment, query)

import           GCounter ()
import           Laws (cvrdtLaws)

test_Cv = cvrdtLaws @(PNCounter Int)

prop_increment = property $ \(counter :: PNCounter Int) i ->
    query (increment i counter) === succ (query counter)

prop_decrement = property $ \(counter :: PNCounter Int) i ->
    query (decrement i counter) === pred (query counter)
