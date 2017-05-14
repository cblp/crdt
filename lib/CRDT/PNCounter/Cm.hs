{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module CRDT.PNCounter.Cm
    ( PNCounter (..)
    , initial
    ) where

import CRDT.Cm (CmRDT, State, update)

-- | Positive-negative counter. Allows incrementing and decrementing.
data PNCounter a = Increment | Decrement

instance Num a => CmRDT (PNCounter a) where
    type State (PNCounter a) = a
    update = \case
        Increment -> (+1)
        Decrement -> subtract 1

-- | Initial state
initial :: Num a => a
initial = 0
