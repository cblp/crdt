{-#LANGUAGE TypeSynonymInstances#-}
module CRDT.GSet.Cv.Iternal where

import           Data.Semigroup (Semigroup ((<>)))
import           Data.Set (Set)
import qualified Data.Set as Set

import CRDT.Cv (CvRDT)

-- | Grow-only set
type GSet = Set

instance Ord a => CvRDT (GSet a)
