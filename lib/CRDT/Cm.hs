module CRDT.Cm
    ( Update (..)
    ) where

data Update atSource downstream = Update
    { atSource :: atSource
    , downstream :: downstream
    }
