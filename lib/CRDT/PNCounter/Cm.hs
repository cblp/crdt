{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module CRDT.PNCounter.Cm
    ( PNCounter (..)
    ) where

import CRDT.Cm (CmRDT, State, initial, update)

-- | Positive-negative counter. Allows incrementing and decrementing.
data PNCounter a = Increment | Decrement

instance Num a => CmRDT (PNCounter a) where
    type State (PNCounter a) = a
    initial _ = 0
    update = \case
        Increment -> (+1)
        Decrement -> subtract 1
