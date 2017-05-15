{-# LANGUAGE TypeFamilies #-}

module CRDT.LWW
    ( LWW (..)
    , Timestamp
    ) where

import Data.Semigroup  (Semigroup (..))
import Numeric.Natural (Natural)

import           CRDT.Cm (CmRDT, State)
import qualified CRDT.Cm as Cm
import           CRDT.Cv (CvRDT)

type Timestamp = Natural

-- | Last write wins. Interesting, this type is both 'CmRDT' and 'CvRDT'.
data LWW a = Write
    { timestamp :: !Timestamp
    , value     :: !a
    }
    deriving (Eq, Ord, Show)

instance Ord a => Semigroup (LWW a) where
    (<>) = max

instance Ord a => CmRDT (LWW a) where
    type State (LWW a) = LWW a
    update = max

instance Ord a => CvRDT (LWW a)
