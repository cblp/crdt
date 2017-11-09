{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module LWW where

import           Control.Monad.State.Strict (StateT, lift)
import           Data.Semigroup ((<>))
import           Data.Set (Set)
import           Test.QuickCheck (Arbitrary, Gen, arbitrary, property, (===))

import           CRDT.LWW (LWW (..), assignP, initialP, query)
import           GlobalTime (Time, runProcess, runSystem)

import           Laws (cmrdtLaw, cvrdtLaws, gen2)
import           QCUtil (genUnique)

prop_Cm =
    cmrdtLaw @(LWW Char) $ Just $ gen2 (genUniquelyTimedValueAndTime, mempty)

test_Cv = cvrdtLaws @(LWW Char) $ Just (genUniquelyTimedLWW, mempty)

prop_assign = property $ \(formerValue :: Char) latterValue -> runSystem $ do
    state1 <- runProcess $ initialP formerValue
    state2 <- runProcess $ assignP latterValue state1
    pure $ query state2 === latterValue

prop_merge_with_former = property $ \(formerValue :: Char) latterValue ->
    runSystem $ do
        state1 <- runProcess $ initialP formerValue
        state2 <- runProcess $ assignP latterValue state1
        pure $ query (state1 <> state2) === latterValue

-- | Generate specified number of 'LWW' with unique timestamps
genUniquelyTimedLWW :: Arbitrary a => StateT (Set Time) Gen (LWW a)
genUniquelyTimedLWW = uncurry LWW <$> genUniquelyTimedValueAndTime

-- | Generate specified number of values with unique timestamps
genUniquelyTimedValueAndTime :: Arbitrary a => StateT (Set Time) Gen (a, Time)
genUniquelyTimedValueAndTime = (,) <$> lift arbitrary <*> genUnique

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)
