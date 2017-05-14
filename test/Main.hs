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

import qualified CRDT.GCounter.Cv          as GCounter
import           CRDT.GCounter.Cv.Internal (GCounter (..))

main :: IO ()
main = defaultMain $ testGroup "" [gCounter]

instance Arbitrary a => Arbitrary (Vector a) where
    arbitrary = Vector.fromList <$> arbitrary

deriving instance Arbitrary a => Arbitrary (GCounter a)

deriving instance Show a => Show (GCounter a)

gCounter :: TestTree
gCounter = testGroup "GCounter"
    [ testGroup "laws"
        [ testProperty "associativity" $
            \(x :: GCounter Int) y z -> (x <> y) <> z == x <> (y <> z)
        , testProperty "commutativity" $
            \(x :: GCounter Int) y -> x <> y == y <> x
        , testProperty "idempotency" $
            \(x :: GCounter Int) -> x <> x == x
        ]
    , testProperty "increment" $
        \(c :: GCounter Int) (Small i) ->
            GCounter.query (GCounter.increment i c) == succ (GCounter.query c)
    ]
