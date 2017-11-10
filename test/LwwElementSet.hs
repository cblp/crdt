{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE TypeApplications #-}

module LwwElementSet
    ( test_Cv
    ) where

import           Control.Monad (replicateM)
import           Control.Monad.State.Strict (StateT, lift)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import           Test.QuickCheck (Arbitrary, Gen, arbitrary)

import           CRDT.Cv.LwwElementSet (LwwElementSet (..))
import           GlobalTime (GlobalTime)

import           Laws (cvrdtLaws)
import           QCUtil (genUnique)

test_Cv = cvrdtLaws @(LwwElementSet Char) $ Just (genUniquelyTimedLES, mempty)

-- | Generate specified number of 'LWW' with unique timestamps
genUniquelyTimedLES
    :: (Arbitrary a, Ord a) => StateT (Set GlobalTime) Gen (LwwElementSet a)
genUniquelyTimedLES = do
    values <- lift arbitrary
    tags <- replicateM (length values) $ (,) <$> genUnique <*> lift arbitrary
    pure $ LES $ Map.fromList $ zip values tags
