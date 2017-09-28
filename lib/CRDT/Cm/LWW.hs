{-# LANGUAGE NamedFieldPuns #-}

module CRDT.Cm.LWW
    ( LWW
    , query
    , updateAtSource
    , updateDownstream
    , before
    ) where

import           Data.Semigroup ((<>))
import           Lens.Micro ((<&>))

import           CRDT (MonadTimestamp (newTimestamp))
import           CRDT.Cv.LWW (query)
import           CRDT.Cv.LWW.Internal

newtype Update a = Assign a

type Intermediate = LWW

updateAtSource
    :: (MonadTimestamp m, Functor m) => Update a -> m (Intermediate a)
updateAtSource (Assign value) =
    newTimestamp <&> \timestamp -> LWW{timestamp = timestamp, value}

updateDownstream :: Intermediate a -> LWW a -> LWW a
updateDownstream = (<>)

before :: a -> a -> Bool
before _ _ = False
