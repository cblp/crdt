{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import           Test.Tasty (TestTree, defaultMain, testGroup)
import           Test.Tasty.QuickCheck (Positive (..), testProperty)

import           CRDT.Cm (update)
import           CRDT.LWW (LWW)
import qualified CRDT.LWW as LWW
import qualified CRDT.PNCounter.Cm as PncCm
import qualified CRDT.PNCounter.Cv as PncCv

import           GCounter (gCounter)
import           Instances ()
import           Laws (cmrdtLaws, cvrdtLaws)

main :: IO ()
main = defaultMain $ testGroup "" [gCounter, pnCounter, lww]

pnCounter :: TestTree
pnCounter = testGroup "PNCounter"
    [ testGroup "Cv"
        [ cvrdtLaws @(PncCv.PNCounter Int)
        , testProperty "increment" $
            \(counter :: PncCv.PNCounter Int) i ->
                PncCv.query (PncCv.increment i counter)
                == succ (PncCv.query counter)
        , testProperty "decrement" $
            \(counter :: PncCv.PNCounter Int) i ->
                PncCv.query (PncCv.decrement i counter)
                == pred (PncCv.query counter)
        ]
    , testGroup "Cm"
        [ cmrdtLaws @(PncCm.PNCounter Int)
        , testProperty "increment" $
            \(counter :: Int) -> update PncCm.Increment counter == succ counter
        , testProperty "decrement" $
            \(counter :: Int) -> update PncCm.Decrement counter == pred counter
        ]
    ]

lww :: TestTree
lww = testGroup "LWW"
    [ testGroup "Cm"
        [ cmrdtLaws @(LWW Int)
        , testProperty "write latter" $
            \formerTime (formerValue :: Int) (Positive dt) latterValue -> let
                latterTime = formerTime + dt
                state = LWW.point formerTime formerValue
                state' = update (LWW.Write latterTime latterValue) state
                in
                LWW.query state' == latterValue
        , testProperty "write former" $
            \formerTime (formerValue :: Int) (Positive dt) latterValue -> let
                latterTime = formerTime + dt
                state = LWW.point latterTime formerValue
                state' = update (LWW.Write formerTime latterValue) state
                in
                LWW.query state' == formerValue
        ]
    , testGroup "Cv"
        [ cvrdtLaws @(LWW Int)
        , testProperty "write latter" $
            \formerTime (formerValue :: Int) (Positive dt) latterValue -> let
                latterTime = formerTime + dt
                state = LWW.point formerTime formerValue
                state' = LWW.write latterTime latterValue state
                in
                LWW.query state' == latterValue
        , testProperty "write former" $
            \formerTime (formerValue :: Int) (Positive dt) latterValue -> let
                latterTime = formerTime + dt
                state = LWW.point latterTime formerValue
                state' = LWW.write formerTime latterValue state
                in
                LWW.query state' == formerValue
        ]
    ]
