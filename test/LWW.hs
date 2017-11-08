{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module LWW where

import           Control.Monad.State.Strict (StateT, gets, lift, modify)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Semigroup ((<>))
import qualified Data.Set as Set
import           Test.QuickCheck (Arbitrary (..), Gen, property, suchThat,
                                  (===))

import           CRDT.LWW (LWW (..), assignP, initialP, query)
import           GlobalTime (Time, runProcess, runSystem)

import           Laws (cmrdtLaw, cvrdtLaws, gen2)

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
genUniquelyTimedLWW :: Arbitrary a => StateT (Map Time a) Gen (LWW a)
genUniquelyTimedLWW = uncurry LWW <$> genUniquelyTimedValueAndTime

-- | Generate specified number of values with unique timestamps
genUniquelyTimedValueAndTime :: Arbitrary a => StateT (Map Time a) Gen (a, Time)
genUniquelyTimedValueAndTime = do
    usedTimestamps <- gets Map.keysSet
    timestamp <- lift $ arbitrary `suchThat` (`Set.notMember` usedTimestamps)
    value <- lift arbitrary
    modify $ Map.insert timestamp value
    pure (value, timestamp)

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)
