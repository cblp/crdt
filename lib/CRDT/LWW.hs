{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module CRDT.LWW
    ( LWW (..)
      -- * CvRDT
    , initial
    , assign
    , query
    ) where

import           Data.Semigroup (Semigroup, (<>))

import           Data.Semilattice (Semilattice)

import           CRDT.Cm (CausalOrd (..), CmRDT (..))
import           CRDT.LamportClock (LamportTime, Process, advance, getTime)

-- | Last write wins. Assuming timestamp is unique.
-- This type is both 'CmRDT' and 'CvRDT'.
--
-- Timestamps are assumed unique, totally ordered,
-- and consistent with causal order;
-- i.e., if assignment 1 happened-before assignment 2,
-- the former’s timestamp is less than the latter’s.
data LWW a = LWW
    { value :: !a
    , time  :: !LamportTime
    }
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- CvRDT -----------------------------------------------------------------------

-- | Merge by choosing more recent timestamp.
instance Eq a => Semigroup (LWW a) where
    x@(LWW xv xt) <> y@(LWW yv yt)
        | xt < yt = y
        | yt < xt = x
        | xv == yv = x
        | otherwise = error "LWW assumes timestamps to be unique"

-- | See 'CvRDT'
instance Eq a => Semilattice (LWW a)

-- | Initialize state
initial :: a -> Process (LWW a)
initial v = LWW v <$> getTime

-- | Change state as CvRDT operation.
-- Current value is ignored, because new timestamp is always greater.
assign :: a -> LWW a -> Process (LWW a)
assign v LWW{time} = do
    advance time
    LWW v <$> getTime

-- | Query state
query :: LWW a -> a
query = value

--------------------------------------------------------------------------------
-- CmRDT -----------------------------------------------------------------------

instance CausalOrd (LWW a) where
    affects _ _ = False

instance Eq a => CmRDT (LWW a) where
    type Intent  (LWW a) = a
    type Payload (LWW a) = LWW a

    makeOp newValue this = Just $ do
        advance thisTimestamp
        newTime <- getTime
        pure LWW{value = newValue, time = newTime}
      where
        LWW{time = thisTimestamp} = this

    apply = (<>)
