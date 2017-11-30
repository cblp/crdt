{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module LwwElementSet
    ( prop_add
    , prop_no_removal_bias
    , prop_remove
    , prop_they_accidentally_delete_our_value
    , test_Cv
    ) where

import           Prelude hiding (lookup)

import           Data.Semigroup ((<>))

import           CRDT.Cv.LwwElementSet (LwwElementSet (..), add, lookup, remove)
import           CRDT.LamportClock (runLamportClock, runProcess)

import           Laws (cvrdtLaws)

test_Cv = cvrdtLaws @(LwwElementSet Char)

prop_add (s :: LwwElementSet Char) x pid1 =
    runLamportClock $ do
        s1 <- runProcess pid1 $ add x s
        pure $ lookup x s1

prop_remove (s :: LwwElementSet Char) x pid1 pid2 =
    runLamportClock $ do
        s1 <- runProcess pid1 $ add x s
        s2 <- runProcess pid2 $ remove x s1
        pure . not $ lookup x s2

-- | Difference from 'TwoPSet' -- no removal bias
prop_no_removal_bias (s :: LwwElementSet Char) x pid1 pid2 pid3 =
    runLamportClock $ do
        s1 <- runProcess pid1 $ add x s
        s2 <- runProcess pid2 $ remove x s1
        s3 <- runProcess pid3 $ add x s2
        pure $ lookup x s3

-- | Difference from 'ORSet' -- other replica can accidentally delete x
prop_they_accidentally_delete_our_value (s :: LwwElementSet Char) x pid1 pid2 =
    runLamportClock $ do
        s1 <- runProcess pid1 $ add x s
        s2 <- runProcess pid2 $ remove x =<< add x s
        pure . not . lookup x $ s1 <> s2
