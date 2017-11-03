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

import           Data.Function (on)
import           Data.Semigroup (Semigroup, (<>))
import           Data.Semilattice (Semilattice)

import           CRDT.Cm (CausalOrd (..), CmRDT (..))
import           LamportClock (Process, Timestamp, newTimestamp)

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
initial :: a -> Timestamp -> LWW a
initial = LWW

initialP :: a -> Process (LWW a)
initialP v = LWW v <$> newTimestamp

-- | Change state as CvRDT operation.
-- Current value is ignored, because new timestamp is always greater.
assign :: a -> LWW a -> Timestamp -> LWW a
assign v _ = LWW v

assignP :: a -> LWW a -> Process (LWW a)
assignP v _ = LWW v <$> newTimestamp

-- | Query state
query :: LWW a -> a
query = value

--------------------------------------------------------------------------------
-- CmRDT -----------------------------------------------------------------------

instance CausalOrd (LWW a) where
    affects _ _ = False

instance Eq a => CmRDT (LWW a) where
    type Intent   (LWW a) = a
    type Payload  (LWW a) = LWW a

    makeOp = LWW

    apply = (<>)
