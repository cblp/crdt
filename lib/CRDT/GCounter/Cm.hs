{-# LANGUAGE TypeFamilies #-}

module CRDT.GCounter.Cm
    ( GCounter (..)
    , initial
    ) where

import           CRDT.Cm (CmRDT, State, update)

-- | Grow-only counter.
--
-- Commutativity: 'Increment' obviously commutes with itself.
data GCounter a = Increment
    deriving (Show)

instance Num a => CmRDT (GCounter a) where
    type State (GCounter a) = a
    update _ = (+1)

-- | Initial state
initial :: Num a => a
initial = 0
