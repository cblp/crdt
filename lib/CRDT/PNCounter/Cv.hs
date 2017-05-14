{-# LANGUAGE NamedFieldPuns #-}

module CRDT.PNCounter.Cv
    ( PNCounter
    , decrement
    , increment
    , query
    ) where

import qualified CRDT.GCounter.Cv as GCounter

import CRDT.PNCounter.Cv.Internal

query :: Num a => PNCounter a -> a
query PNCounter{positive, negative} =
    GCounter.query positive - GCounter.query negative

decrement :: Num a => Word -> PNCounter a -> PNCounter a
decrement i pnc@PNCounter{negative} =
    pnc{negative = GCounter.increment i negative}

increment :: Num a => Word -> PNCounter a -> PNCounter a
increment i pnc@PNCounter{positive} =
    pnc{positive = GCounter.increment i positive}
