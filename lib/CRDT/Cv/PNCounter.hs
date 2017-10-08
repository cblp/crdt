{-# LANGUAGE NamedFieldPuns #-}

module CRDT.Cv.PNCounter
    ( PNCounter (..)
    , initial
    , query
    -- * Operations
    , decrement
    , increment
    ) where

import           Data.Semigroup (Semigroup (..))
import           Data.Semilattice (Semilattice)

import           CRDT.Cv.GCounter (GCounter)
import qualified CRDT.Cv.GCounter as GCounter

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

-- | See 'CvRDT'
instance Ord a => Semilattice (PNCounter a)

-- | Get value from the state
query :: Num a => PNCounter a -> a
query PNCounter{positive, negative} =
    GCounter.query positive - GCounter.query negative

-- | Decrement counter
decrement
    :: Num a
    => Word -- ^ replica id
    -> PNCounter a
    -> PNCounter a
decrement i pnc@PNCounter{negative} =
    pnc{negative = GCounter.increment i negative}

-- | Increment counter
increment
    :: Num a
    => Word -- ^ replica id
    -> PNCounter a
    -> PNCounter a
increment i pnc@PNCounter{positive} =
    pnc{positive = GCounter.increment i positive}

-- | Initial state
initial :: PNCounter a
initial = PNCounter{positive = GCounter.initial, negative = GCounter.initial}
