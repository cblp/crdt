{-# LANGUAGE NamedFieldPuns #-}

module CRDT.Cm.LWW
    ( LWW (..)
    , Update
    , Intermediate
    , query
    , updateAtSource
    , updateDownstream
    , before
    ) where

import           Data.Semigroup ((<>))
import           Lens.Micro ((<&>))

import           CRDT.Cv.LWW (LWW (..), query)
import           LamportClock (Clock (newTimestamp))

newtype Update a = Assign a

type Intermediate = LWW

updateAtSource :: Clock f => Update a -> f (Intermediate a)
updateAtSource (Assign value) =
    newTimestamp <&> \timestamp -> LWW{timestamp = timestamp, value}

updateDownstream :: Intermediate a -> LWW a -> LWW a
updateDownstream = (<>)

before :: a -> a -> Bool
before _ _ = False
