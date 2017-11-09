module QCUtil
    ( genUnique
    ) where

import           Control.Monad.State.Strict (StateT, get, lift, modify)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Test.QuickCheck (Arbitrary, Gen, arbitrary, suchThat)

genUnique :: (Arbitrary a, Ord a) => StateT (Set a) Gen a
genUnique = do
    used <- get
    a <- lift $ arbitrary `suchThat` (`Set.notMember` used)
    modify $ Set.insert a
    pure a
