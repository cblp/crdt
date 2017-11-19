{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module PNCounter where

import           Test.QuickCheck ((===))

import           CRDT.Cv.PNCounter (PNCounter (..), decrement, increment, query)

import           GCounter ()
import           Laws (cvrdtLaws)

test_Cv = cvrdtLaws @(PNCounter Int) Nothing

prop_increment (counter :: PNCounter Int) pid =
    query (increment pid counter) === succ (query counter)

prop_decrement (counter :: PNCounter Int) pid =
    query (decrement pid counter) === pred (query counter)
