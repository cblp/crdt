{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module LWW
    ( genUniquelyTimedLWW
    , prop_Cm
    , prop_assign
    , prop_merge_with_former
    , test_Cv
    ) where

import           Control.Monad.State.Strict (StateT, get, lift)
import           Data.Semigroup ((<>))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Test.QuickCheck (Arbitrary, Gen, arbitrary, counterexample,
                                  property, (===))

import           CRDT.HybridClock (HybridClock (..), HybridTime, runHybridClock,
                                   runProcess)
import           CRDT.LWW (LWW (..), assign, initial, query)

import           Laws (cmrdtLaw, cvrdtLaws)
import           QCUtil (genUnique)

prop_Cm = cmrdtLaw @(LWW Char)

test_Cv = cvrdtLaws @(LWW Char) $ Just (genUniquelyTimedLWW, Set.empty)

prop_assign = property $ \pid1 pid2 (formerValue :: Char) latterValue ->
    runHybridClock $ do
        state1 <- runProcess pid1 $ initial formerValue
        state2 <- runProcess pid2 $ assign latterValue state1
        pure $ query state2 === latterValue

prop_merge_with_former = property $
    \pid1 pid2 (formerValue :: Char) latterValue ->
    runHybridClock $ do
        state1 <- runProcess pid1 $ initial formerValue
        state2 <- runProcess pid2 $ assign latterValue state1
        clock <- HybridClock get
        pure $
            counterexample ("state1 = " ++ show state1) $
            counterexample ("state2 = " ++ show state2) $
            counterexample ("clock = " ++ show clock) $
            query (state1 <> state2) === latterValue

-- | Generate specified number of 'LWW' with unique timestamps
genUniquelyTimedLWW :: Arbitrary a => StateT (Set HybridTime) Gen (LWW a)
genUniquelyTimedLWW = LWW <$> lift arbitrary <*> genUnique
