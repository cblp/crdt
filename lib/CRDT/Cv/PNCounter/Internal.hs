module CRDT.Cv.PNCounter.Internal where

import           Data.Semigroup (Semigroup (..))

import           CRDT.Cv (CvRDT)
import           CRDT.Cv.GCounter (GCounter)

{- |
Positive-negative counter. Allows incrementing and decrementing.
Nice example of combining of existing CvRDT ('GCounter' in this case)
to create another CvRDT.
-}
data PNCounter a = PNCounter
    { positive :: !(GCounter a)
    , negative :: !(GCounter a)
    }
    deriving (Eq, Show)

instance Ord a => Semigroup (PNCounter a) where
    PNCounter p1 n1 <> PNCounter p2 n2 = PNCounter (p1 <> p2) (n1 <> n2)

instance Ord a => CvRDT (PNCounter a)
