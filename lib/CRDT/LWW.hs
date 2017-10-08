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

import           Data.Function (on)
import           Data.Semigroup (Semigroup, (<>))
import           Data.Semilattice (Semilattice)

import           CRDT.Cm (CausalOrd (..), CmRDT (..))
import           LamportClock (Clock (newTimestamp), Timestamp)

-- | Last write wins. Assuming timestamp is unique.
-- This type is both 'CmRDT' and 'CvRDT'.
data LWW a = LWW
    { value     :: !a
    , timestamp :: !Timestamp
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

-- | See 'CvRDT'
instance Semilattice (LWW a)

-- | Initialize state
initial :: Clock f => a -> f (LWW a)
initial value = LWW value <$> newTimestamp

-- | Change state as CvRDT operation.
-- Current value is ignored, because new timestamp is always greater.
assign :: Clock f => a -> LWW a -> f (LWW a)
assign value _ = LWW value <$> newTimestamp

-- | Query state
query :: LWW a -> a
query = value

--------------------------------------------------------------------------------
-- CmRDT -----------------------------------------------------------------------

instance CausalOrd (LWW a) where
    before _ _ = False

-- | Change state as CmRDT operation
newtype Assign a = Assign a
    deriving (Eq, Show)

instance Eq a => CmRDT (LWW a) where
    type Op       (LWW a) = Assign a
    type Payload  (LWW a) = LWW a
    type View     (LWW a) = a

    updateAtSource (Assign value) = LWW value <$> newTimestamp

    updateDownstream = (<>)

    view = value
