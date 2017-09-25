{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import           Data.Semigroup (Semigroup, (<>))
import           Data.Semilattice (Semilattice, slappend)
import           Test.QuickCheck (Arbitrary)
import           Test.Tasty (TestTree, defaultMain, testGroup)
import           Test.Tasty.QuickCheck (Positive (..), testProperty)

import           CRDT.Cm (CmRDT, State, update)
import           CRDT.Cv (CvRDT)
import qualified CRDT.GCounter.Cm as GcCm
import qualified CRDT.GCounter.Cv as GcCv
import           CRDT.LWW (LWW)
import qualified CRDT.LWW as LWW
import qualified CRDT.PNCounter.Cm as PncCm
import qualified CRDT.PNCounter.Cv as PncCv

import           Instances ()

main :: IO ()
main = defaultMain $ testGroup "" [gCounter, pnCounter, lww]

gCounter :: TestTree
gCounter = testGroup "GCounter"
    [ testGroup "Cv"
        [ cvrdtLaws @(GcCv.GCounter Int)
        , testProperty "increment" $
            \(counter :: GcCv.GCounter Int) i ->
                GcCv.query (GcCv.increment i counter)
                == succ (GcCv.query counter)
        ]
    , testGroup "Cm"
        [ cmrdtLaws @(GcCm.GCounter Int)
        , testProperty "increment" $
            \(counter :: Int) -> update GcCm.Increment counter == succ counter
        ]
    ]

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

semigroupLaw :: forall a . (Arbitrary a, Semigroup a, Eq a, Show a) => TestTree
semigroupLaw =
    testGroup "Semigroup law" [testProperty "associativity" associativity]
  where
    associativity :: a -> a -> a -> Bool
    associativity x y z = (x <> y) <> z == x <> (y <> z)

semilatticeLaws
    :: forall a . (Arbitrary a, Semilattice a, Eq a, Show a) => TestTree
semilatticeLaws = testGroup "Semilattice laws"
    [ semigroupLaw @a
    , testProperty "commutativity" commutativity
    , testProperty "idempotency"   idempotency
    ]
  where
    commutativity :: a -> a -> Bool
    commutativity x y = x `slappend` y == y `slappend` x

    idempotency :: a -> Bool
    idempotency x = x `slappend` x == x

cvrdtLaws :: forall a . (Arbitrary a, CvRDT a, Eq a, Show a) => TestTree
cvrdtLaws = semilatticeLaws @a

cmrdtLaws
    :: forall op
    . ( Arbitrary op, CmRDT op, Show op
      , Arbitrary (State op), Eq (State op), Show (State op)
      )
    => TestTree
cmrdtLaws = testProperty "CmRDT law: commutativity" commutativity
  where
    commutativity :: op -> op -> State op -> Bool
    commutativity op1 op2 x =
        (update op1 . update op2) x == (update op2 . update op1) x
