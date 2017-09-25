{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module LWW (lww) where

import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (Positive (..), testProperty)

import           CRDT.Cm (update)
import           CRDT.LWW (LWW (Write), point, query, write)

import           Laws (cmrdtLaws, cvrdtLaws)

instance Arbitrary a => Arbitrary (LWW a) where
    arbitrary = Write <$> arbitrary <*> arbitrary

lww :: TestTree
lww = testGroup "LWW"
    [ testGroup "Cm"
        [ cmrdtLaws @(LWW Int)
        , testProperty "write latter" $
            \formerTime (formerValue :: Int) (Positive dt) latterValue -> let
                latterTime = formerTime + dt
                state = point formerTime formerValue
                state' = update (Write latterTime latterValue) state
                in
                query state' == latterValue
        , testProperty "write former" $
            \formerTime (formerValue :: Int) (Positive dt) latterValue -> let
                latterTime = formerTime + dt
                state = point latterTime latterValue
                state' = update (Write formerTime formerValue) state
                in
                query state' == latterValue
        ]
    , testGroup "Cv"
        [ cvrdtLaws @(LWW Int)
        , testProperty "write latter" $
            \formerTime (formerValue :: Int) (Positive dt) latterValue -> let
                latterTime = formerTime + dt
                state = point formerTime formerValue
                state' = write latterTime latterValue state
                in
                query state' == latterValue
        , testProperty "write former" $
            \formerTime (formerValue :: Int) (Positive dt) latterValue -> let
                latterTime = formerTime + dt
                state = point latterTime latterValue
                state' = write formerTime formerValue state
                in
                query state' == latterValue
        ]
    ]
