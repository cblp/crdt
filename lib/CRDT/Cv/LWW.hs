{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module CRDT.Cv.LWW
    ( LWW (..)
    , initial
    , assign
    , query
    ) where

import           Data.Function (on)
import           Data.Semigroup (Semigroup, (<>))
import           Lens.Micro ((<&>))

import           CRDT.Cv (CvRDT)
import           LamportClock (Clock (newTimestamp), Timestamp)

-- | Last write wins. Assuming timestamp is unique.
data LWW a = LWW
    { timestamp :: !Timestamp
    , value     :: !a
    }
    deriving (Show)

instance Eq (LWW a) where
    (==) = (==) `on` timestamp

instance Ord (LWW a) where
    (<=) = (<=) `on` timestamp

-- | Merge by choosing more recent timestamp.
instance Semigroup (LWW a) where
    (<>) = max

instance CvRDT (LWW a)

-- | Initialize state
initial :: Clock f => a -> f (LWW a)
initial value = newTimestamp <&> \timestamp -> LWW{timestamp = timestamp, value}

-- | Change state
assign :: Clock f => a -> LWW a -> f (LWW a)
assign value cur = newTimestamp <&> \timestamp -> cur <> LWW{timestamp, value}

-- | Query state
query :: LWW a -> a
query = value
