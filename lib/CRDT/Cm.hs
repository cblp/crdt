{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module CRDT.Cm
    ( CmRDT (..)
    , concurrent
    ) where

import           Algebra.PartialOrd (PartialOrd (leq))
import           Data.Observe (Observe (..))

import           LamportClock (Clock)

-- | TODO(cblp, 2017-09-29) import from lattices >= 1.6
comparable :: PartialOrd a => a -> a -> Bool
comparable a b = a `leq` b || b `leq` a

concurrent :: PartialOrd a => a -> a -> Bool
concurrent a b = not $ comparable a b

class (Observe payload, Eq (Observed payload), PartialOrd up)
        => CmRDT payload op up | payload -> op, op -> up, up -> payload where

    -- | Precondition
    updateAtSourcePre :: op -> payload -> Bool
    updateAtSourcePre _ _ = True

    updateAtSource :: Clock m => op -> m up

    updateDownstream :: up -> payload -> payload
