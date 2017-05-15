{-# LANGUAGE NamedFieldPuns #-}

module CRDT.PNCounter.Cv
    ( PNCounter
    , initial
    , query
    -- * Operations
    , decrement
    , increment
    ) where

import qualified CRDT.GCounter.Cv as GCounter

import CRDT.PNCounter.Cv.Internal

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
