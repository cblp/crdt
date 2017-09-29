{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module CRDT.Cv.LWW
    ( LWW
    , initial
    , assign
    , query
    ) where

import           Data.Semigroup ((<>))
import           Lens.Micro ((<&>))

import           CRDT.Cv.LWW.Internal
import           LamportClock (Clock (newTimestamp))

-- | Initialize state
initial :: Clock f => a -> f (LWW a)
initial value = newTimestamp <&> \timestamp -> LWW{timestamp = timestamp, value}

-- | Change state
assign :: Clock f => a -> LWW a -> f (LWW a)
assign value cur = newTimestamp <&> \timestamp -> cur <> LWW{timestamp, value}

-- | Query state
query :: LWW a -> a
query = value
