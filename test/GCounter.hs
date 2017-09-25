{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module GCounter
    ( gCounter
    ) where

import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           CRDT.Cm (update)
import qualified CRDT.GCounter.Cm as Cm
import qualified CRDT.GCounter.Cv as Cv
import qualified CRDT.GCounter.Cv.Internal as Cv

import           Laws (cmrdtLaws, cvrdtLaws)

instance Arbitrary (Cm.GCounter a) where
    arbitrary = pure Cm.Increment

deriving instance Arbitrary a => Arbitrary (Cv.GCounter a)

gCounter :: TestTree
gCounter = testGroup "GCounter"
    [ testGroup "Cv"
        [ cvrdtLaws @(Cv.GCounter Int)
        , testProperty "increment" $
            \(counter :: Cv.GCounter Int) i ->
                Cv.query (Cv.increment i counter)
                == succ (Cv.query counter)
        ]
    , testGroup "Cm"
        [ cmrdtLaws @(Cm.GCounter Int)
        , testProperty "increment" $
            \(counter :: Int) -> update Cm.Increment counter == succ counter
        ]
    ]
