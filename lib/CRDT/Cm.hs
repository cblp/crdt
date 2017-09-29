module CRDT.Cm
    ( UpdateHandler (..)
    ) where

data UpdateHandler atSource downstream = UpdateHandler
    { atSource :: atSource
    , downstream :: downstream
    }
