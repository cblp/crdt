{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Data.Proxy (Proxy (..))
import           Data.Semilattice (Semilattice, (<>))
import           Test.QuickCheck (Arbitrary, Small (..))
import           Test.Tasty (TestTree, defaultMain, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           CRDT.Cm (CmRDT, State)
import qualified CRDT.Cm as Cm
import           CRDT.Cv (CvRDT)
import qualified CRDT.GCounter.Cv as GcCv
import           CRDT.LWW (LWW)
import qualified CRDT.PNCounter.Cm as PncCm
import qualified CRDT.PNCounter.Cv as PncCv

import           Instances ()

main :: IO ()
main = defaultMain $ testGroup "" [gCounter, pnCounter, lww]

gCounter :: TestTree
gCounter = testGroup "GCounter"
    [ testGroup "Cv"
        [ cvrdtLaws (Proxy :: Proxy (GcCv.GCounter Int))
        , testProperty "increment" $
            \(c :: GcCv.GCounter Int) (Small i) ->
                GcCv.query (GcCv.increment i c) == succ (GcCv.query c)
        ]
    ]

pnCounter :: TestTree
pnCounter = testGroup "PNCounter"
    [ testGroup "Cv"
        [ cvrdtLaws (Proxy :: Proxy (PncCv.PNCounter Int))
        , testProperty "increment" $
            \(c :: PncCv.PNCounter Int) (Small i) ->
                PncCv.query (PncCv.increment i c) == succ (PncCv.query c)
        , testProperty "decrement" $
            \(c :: PncCv.PNCounter Int) (Small i) ->
                PncCv.query (PncCv.decrement i c) == pred (PncCv.query c)
        ]
    , testGroup "Cm"
        [ cmrdtLaws (Proxy :: Proxy (PncCm.PNCounter Int)) ]
    ]

lww :: TestTree
lww = testGroup "LWW"
    [ testGroup "Cm" [ cmrdtLaws (Proxy :: Proxy (LWW Int)) ]
    , testGroup "Cv" [ cvrdtLaws (Proxy :: Proxy (LWW Int)) ]
    ]

semilatticeLaws
    :: forall a . (Arbitrary a, CvRDT a, Eq a, Show a) => Proxy a -> TestTree
semilatticeLaws _ = testGroup "Semilattice laws"
    [ testProperty "associativity" associativity
    , testProperty "commutativity" commutativity
    , testProperty "idempotency"   idempotency
    ]
  where
    associativity :: a -> a -> a -> Bool
    associativity x y z = (x <> y) <> z == x <> (y <> z)

    commutativity :: a -> a -> Bool
    commutativity x y = x <> y == y <> x

    idempotency :: a -> Bool
    idempotency x = x <> x == x

cvrdtLaws :: (Arbitrary a, Semilattice a, Eq a, Show a) => Proxy a -> TestTree
cvrdtLaws = semilatticeLaws

cmrdtLaws
    :: forall op
    . ( Arbitrary op, CmRDT op, Show op
      , Arbitrary (State op), Eq (State op), Show (State op)
      )
    => Proxy op -> TestTree
cmrdtLaws _ = testProperty "CmRDT law: commutativity" commutativity
  where
    commutativity :: op -> op -> State op -> Bool
    commutativity op1 op2 x =
        (Cm.update op1 . Cm.update op2) x == (Cm.update op2 . Cm.update op1) x
