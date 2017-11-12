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

import           Control.Monad (replicateM)
import           Control.Monad.State.Strict (StateT, lift)
import qualified Data.Map.Strict as Map
import           Data.Semigroup ((<>))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Test.QuickCheck (Arbitrary, Gen, arbitrary)

import           CRDT.Cv.LwwElementSet (LwwElementSet (..), add, lookup, remove)
import           CRDT.LamportClock (LamportTime, runLamportClock, runProcess)

import           Laws (cvrdtLaws)
import           LWW (genUniquelyTimedLWW)

test_Cv =
    cvrdtLaws @(LwwElementSet Char) $ Just (genUniquelyTimedLES, Set.empty)

-- | Generate specified number of 'LWW' with unique timestamps
genUniquelyTimedLES
    :: (Arbitrary a, Ord a) => StateT (Set LamportTime) Gen (LwwElementSet a)
genUniquelyTimedLES = do
    values <- lift arbitrary
    tags <- replicateM (length values) genUniquelyTimedLWW
    pure $ LES $ Map.fromList $ zip values tags

prop_add (s :: LwwElementSet Char) x pid1 =
    runLamportClock $ do
        s1 <- runProcess pid1 $ add x s
        pure $ lookup x s1

prop_remove (s :: LwwElementSet Char) x pid1 pid2 =
    runLamportClock $ do
        s1 <- runProcess pid1 $ add x s
        s2 <- runProcess pid2 $ remove x s1
        pure . not $ lookup x s2

-- | Difference from TwoPSet -- no removal bias
prop_no_removal_bias (s :: LwwElementSet Char) x pid1 pid2 pid3 =
    runLamportClock $ do
        s1 <- runProcess pid1 $ add x s
        s2 <- runProcess pid2 $ remove x s1
        s3 <- runProcess pid3 $ add x s2
        pure $ lookup x s3

-- | Difference from ORSet -- other replica can accidentally delete x
prop_they_accidentally_delete_our_value (s :: LwwElementSet Char) x pid1 pid2 =
    runLamportClock $ do
        s1 <- runProcess pid1 $ add x s
        s2 <- runProcess pid2 $ remove x =<< add x s
        pure . not . lookup x $ s1 <> s2
