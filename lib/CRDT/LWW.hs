{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module CRDT.LWW
    ( LWW (..)
      -- * CvRDT
    , initial
    , assign
    , query
      -- * CmRDT
    , Assign (..)
    ) where

import           Algebra.PartialOrd (PartialOrd (..))
import           Data.Function (on)
import           Data.Proxy (Proxy (..))
import           Data.Semigroup (Semigroup, (<>))
import           Lens.Micro ((<&>))

import           CRDT.Cm (CmRDT (..))
import           CRDT.Cv (CvRDT)
import           LamportClock (Clock (newTimestamp), Timestamp)

-- | Last write wins. Assuming timestamp is unique.
data LWW a = LWW
    { timestamp :: !Timestamp
    , value     :: !a
    }
    deriving (Show)

--------------------------------------------------------------------------------
-- CvRDT -----------------------------------------------------------------------

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

-- | Change state as CvRDT operation
assign :: Clock f => a -> LWW a -> f (LWW a)
assign value cur = newTimestamp <&> \timestamp -> cur <> LWW{timestamp, value}

-- | Query state
query :: LWW a -> a
query = value

--------------------------------------------------------------------------------
-- CmRDT -----------------------------------------------------------------------

instance PartialOrd (LWW a) where
    leq _ _ = False

-- | Change state as CmRDT operation
newtype Assign a = Assign a
    deriving (Eq, Show)

instance Eq a => CmRDT (LWW a) where
    type Op       (LWW a) = Assign a
    type Payload  (LWW a) = LWW a
    type View     (LWW a) = a

    updateAtSource (Assign value) =
        newTimestamp <&> \t -> LWW{timestamp = t, value}

    updateDownstream = (<>)

    view Proxy = value
