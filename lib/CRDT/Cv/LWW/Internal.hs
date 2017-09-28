module CRDT.Cv.LWW.Internal where

import           Data.Function (on)
import           Data.Semigroup (Semigroup, (<>))

import           CRDT (Timestamp)
import           CRDT.Cv (CvRDT)

-- | Last write wins. Assuming timestamp is unique.
data LWW a = LWW
    { timestamp :: !Timestamp
    , value     :: !a
    }
    deriving (Show)

instance Eq (LWW a) where
    (==) = (==) `on` timestamp

instance Ord (LWW a) where
    (<=) = (<=) `on` timestamp

-- | Merge by choosing more recent timestamp.
instance Semigroup (LWW a) where
    (<>) = max

instance CvRDT (LWW a)
