{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE TypeApplications #-}

module LwwElementSet
    ( test_Cv
    ) where

import           Control.Monad (replicateM)
import           Control.Monad.State.Strict (StateT, lift)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Test.QuickCheck (Arbitrary, Gen, arbitrary)

import           CRDT.Cv.LwwElementSet (LwwElementSet (..))
import           CRDT.HybridClock (HybridTime)

import           Laws (cvrdtLaws)
import           QCUtil (genUnique)

test_Cv =
    cvrdtLaws @(LwwElementSet Char) $ Just (genUniquelyTimedLES, Set.empty)

-- | Generate specified number of 'LWW' with unique timestamps
genUniquelyTimedLES
    :: (Arbitrary a, Ord a) => StateT (Set HybridTime) Gen (LwwElementSet a)
genUniquelyTimedLES = do
    values <- lift arbitrary
    tags <- replicateM (length values) $ (,) <$> genUnique <*> lift arbitrary
    pure $ LES $ Map.fromList $ zip values tags
