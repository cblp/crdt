{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module CRDT.LWW
    ( LWW (..)
      -- * CvRDT
    , initial
    , initialP
    , assign
    , assignP
    , query
    ) where

import           Control.Monad (guard)
import           Data.Semigroup (Semigroup, (<>))
import           Data.Semilattice (Semilattice)

import           CRDT.Cm (CausalOrd (..), CmRDT (..))
import           GlobalTime (GlobalTime, Process, newTime)

-- | Last write wins. Assuming timestamp is unique.
-- This type is both 'CmRDT' and 'CvRDT'.
--
-- Timestamps are assumed unique, totally ordered,
-- and consistent with causal order;
-- i.e., if assignment 1 happened-before assignment 2,
-- the former’s timestamp is less than the latter’s.
data LWW a = LWW
    { value     :: !a
    , timestamp :: !GlobalTime
    }
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- CvRDT -----------------------------------------------------------------------

-- | Merge by choosing more recent timestamp.
instance Eq a => Semigroup (LWW a) where
    x@(LWW xv xt) <> y@(LWW yv yt) =
        case compare xt yt of
            LT -> y
            GT -> x
            EQ ->
                if xv == yv then
                    x
                else
                    error "LWW assumes timestamps to be unique"

-- | See 'CvRDT'
instance Eq a => Semilattice (LWW a)

-- | Initialize state
initial :: a -> GlobalTime -> LWW a
initial = LWW

initialP :: a -> Process (LWW a)
initialP v = LWW v <$> newTime

-- | Change state as CvRDT operation.
-- Current value is ignored, because new timestamp is always greater.
assign :: a -> LWW a -> GlobalTime -> LWW a
assign v _ = LWW v

assignP :: a -> LWW a -> Process (LWW a)
assignP v _ = LWW v <$> newTime

-- | Query state
query :: LWW a -> a
query = value

--------------------------------------------------------------------------------
-- CmRDT -----------------------------------------------------------------------

instance CausalOrd (LWW a) where
    affects _ _ = False

instance Eq a => CmRDT (LWW a) where
    type Intent   (LWW a) = (a, GlobalTime)
    type Payload  (LWW a) = LWW a

    makeOp (newValue, newTimestamp) cur =
        guard (newTimestamp > curTimestamp) *> Just new
      where
        LWW{timestamp = curTimestamp} = cur
        new = LWW{value = newValue, timestamp = newTimestamp}

    apply = (<>)
