{-# LANGUAGE TypeFamilies #-}

module CRDT.GCounter.Cm
    ( GCounter (..)
    ) where

import CRDT.Cm (CmRDT, State, initial, update)

-- | 'Increment' obviously commutes with itself.
data GCounter a = Increment

instance Num a => CmRDT (GCounter a) where
    type State (GCounter a) = a
    initial _ = 0
    update _ = (+1)
