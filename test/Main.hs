{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

import           Data.Semigroup        ((<>))
import           Data.Vector           (Vector)
import qualified Data.Vector           as Vector
import           Test.QuickCheck       (Arbitrary, Small (..), arbitrary)
import           Test.Tasty            (TestTree, defaultMain, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import qualified CRDT.GCounter.Cv           as GcCv
import qualified CRDT.GCounter.Cv.Internal  as GcCv
import qualified CRDT.PNCounter.Cv          as PncCv
import qualified CRDT.PNCounter.Cv.Internal as PncCv

main :: IO ()
main = defaultMain $ testGroup "" [gCounter, pnCounter]

instance Arbitrary a => Arbitrary (Vector a) where
    arbitrary = Vector.fromList <$> arbitrary

deriving instance Arbitrary a => Arbitrary (GcCv.GCounter a)

deriving instance Show a => Show (GcCv.GCounter a)

instance Arbitrary a => Arbitrary (PncCv.PNCounter a) where
    arbitrary = PncCv.PNCounter <$> arbitrary <*> arbitrary

deriving instance Show a => Show (PncCv.PNCounter a)

gCounter :: TestTree
gCounter = testGroup "GCounter"
    [ testGroup "Cv"
        [ testGroup "laws"
            [ testProperty "associativity" $
                \(x :: GcCv.GCounter Int) y z -> (x <> y) <> z == x <> (y <> z)
            , testProperty "commutativity" $
                \(x :: GcCv.GCounter Int) y -> x <> y == y <> x
            , testProperty "idempotency" $
                \(x :: GcCv.GCounter Int) -> x <> x == x
            ]
        , testProperty "increment" $
            \(c :: GcCv.GCounter Int) (Small i) ->
                GcCv.query (GcCv.increment i c) == succ (GcCv.query c)
        ]
    ]

pnCounter :: TestTree
pnCounter = testGroup "PNCounter"
    [ testGroup "Cv"
        [ testGroup "laws"
            [ testProperty "associativity" $
                \(x :: PncCv.PNCounter Int) y z ->
                    (x <> y) <> z == x <> (y <> z)
            , testProperty "commutativity" $
                \(x :: PncCv.PNCounter Int) y -> x <> y == y <> x
            , testProperty "idempotency" $
                \(x :: PncCv.PNCounter Int) -> x <> x == x
            ]
        , testProperty "increment" $
            \(c :: PncCv.PNCounter Int) (Small i) ->
                PncCv.query (PncCv.increment i c) == succ (PncCv.query c)
        , testProperty "decrement" $
            \(c :: PncCv.PNCounter Int) (Small i) ->
                PncCv.query (PncCv.decrement i c) == pred (PncCv.query c)
        ]
    ]
