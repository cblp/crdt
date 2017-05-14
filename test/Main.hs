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

import qualified CRDT.GCounter.Cv          as GcCv
import qualified CRDT.GCounter.Cv.Internal as GcCv

main :: IO ()
main = defaultMain $ testGroup "" [gCounter]

instance Arbitrary a => Arbitrary (Vector a) where
    arbitrary = Vector.fromList <$> arbitrary

deriving instance Arbitrary a => Arbitrary (GcCv.GCounter a)

deriving instance Show a => Show (GcCv.GCounter a)

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
