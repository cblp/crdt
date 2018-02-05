{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module CRDT.LWW
    ( LWW (..)
      -- * CvRDT
    , initialize
    , assign
    , query
      -- * Implementation detail
    , advanceFromLWW
    ) where

import           Data.Semigroup (Semigroup, (<>))

import           Data.Semilattice (Semilattice)

import           CRDT.Cm (CausalOrd (..), CmRDT (..))
import           CRDT.LamportClock (Clock, LamportTime (LamportTime), advance,
                                    getTime)

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
initialize :: Clock m => a -> m (LWW a)
initialize value = LWW value <$> getTime

-- | Change state as CvRDT operation.
-- Current value is ignored, because new timestamp is always greater.
assign :: Clock m => a -> LWW a -> m (LWW a)
assign value old = do
    advanceFromLWW old
    initialize value

-- | Query state
query :: LWW a -> a
query = value

--------------------------------------------------------------------------------
-- CmRDT -----------------------------------------------------------------------

instance CausalOrd (LWW a) where
    precedes _ _ = False

instance Eq a => CmRDT (LWW a) where
    type Intent  (LWW a) = a
    type Payload (LWW a) = Maybe (LWW a)

    initial = Nothing

    makeOp value = Just . \case
        Just payload -> assign value payload
        Nothing      -> initialize value

    apply op = Just . \case
        Just payload -> op <> payload
        Nothing      -> op

advanceFromLWW :: Clock m => LWW a -> m ()
advanceFromLWW LWW{time = LamportTime time _} = advance time
