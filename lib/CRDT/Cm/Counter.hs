{-# LANGUAGE LambdaCase #-}

module CRDT.Cm.Counter
    ( Counter (..)
    , updateDownstream
    , before
    ) where

data Counter = Increment | Decrement

updateDownstream :: Enum a => Counter -> a -> a
updateDownstream = \case
    Increment -> succ
    Decrement -> pred

before :: a -> a -> Bool
before _ _ = False
