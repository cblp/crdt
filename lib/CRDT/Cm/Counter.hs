{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module CRDT.Cm.Counter
    ( Counter (..)
    ) where

import           CRDT.Cm (CausalOrd (..), CmRDT (..))

data Counter a = Increment | Decrement
    deriving (Bounded, Enum, Eq, Show)

instance (Num a, Eq a) => CmRDT (Counter a) where
    type Payload (Counter a) = a

    apply = \case
        Increment -> (+ 1)
        Decrement -> subtract 1

-- | Empty order, allowing arbitrary reordering
instance CausalOrd (Counter a) where
    affects _ _ = False
