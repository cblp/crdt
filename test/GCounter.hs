{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module GCounter where

import           Test.QuickCheck ((===))

import           CRDT.Cv.GCounter (GCounter (..), increment, query)

import           Laws (cvrdtLaws)

test_Cv = cvrdtLaws @(GCounter Int)

prop_increment (counter :: GCounter Int) pid =
    query (increment pid counter) === succ (query counter)
