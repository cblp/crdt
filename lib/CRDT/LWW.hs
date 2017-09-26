{-# LANGUAGE TypeFamilies #-}

module CRDT.LWW
    ( LWW (..)
    , point
    , write
    , query
    ) where

import           Data.Semigroup (Semigroup (..))

import           CRDT.Cm (CmRDT, State)
import qualified CRDT.Cm as Cm
import           CRDT.Cv (CvRDT)
import           CRDT.Timestamp (Timestamp)

-- | Last write wins. Interesting, this type is both 'CmRDT' and 'CvRDT'.
data LWW a = Write
    { timestamp :: !Timestamp
    , value     :: !a
    }
    deriving (Eq, Ord, Show)

-- | Merge by choosing more recent timestamp.
instance Ord a => Semigroup (LWW a) where
    (<>) = max

instance Ord a => CmRDT (LWW a) where
    type State (LWW a) = LWW a
    update = max

instance Ord a => CvRDT (LWW a)

-- | Initialize state
point :: Timestamp -> a -> LWW a
point = Write

-- | Change state
write :: Ord a => Timestamp -> a -> LWW a -> LWW a
write t v s = Write t v <> s

-- | Query state
query :: LWW a -> a
query = value
