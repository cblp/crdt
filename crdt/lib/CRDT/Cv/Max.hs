module CRDT.Cv.Max
    ( Max (..)
    , initial
    , query
    ) where

import           Data.Semigroup (Max (..))

-- | Construct new value
initial :: a -> Max a
initial = Max

query :: Max a -> a
query = getMax
