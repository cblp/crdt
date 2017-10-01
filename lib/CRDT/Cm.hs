{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module CRDT.Cm
    ( CmRDT (..)
    , concurrent
    , updateLocally
    ) where

import           Algebra.PartialOrd (PartialOrd (leq))
import           Lens.Micro ((<&>))

import           LamportClock (Clock)

-- | TODO(cblp, 2017-09-29) import from lattices >= 1.6
comparable :: PartialOrd a => a -> a -> Bool
comparable a b = a `leq` b || b `leq` a

concurrent :: PartialOrd a => a -> a -> Bool
concurrent a b = not $ comparable a b

class (PartialOrd up, Eq view) => CmRDT state op up view
        | state -> op, op -> state, op -> up, state -> view where
    updateAtSource :: Clock m => op -> m up
    updateDownstream :: up -> state -> state
    view :: state -> view

updateLocally :: (CmRDT state op up view, Clock m) => op -> state -> m state
updateLocally op state = updateAtSource op <&> \up -> updateDownstream up state
