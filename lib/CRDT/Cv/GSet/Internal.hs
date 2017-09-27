{-# OPTIONS_GHC -Wno-orphans #-}

module CRDT.Cv.GSet.Internal where

import           Data.Set (Set)

import           CRDT.Cv (CvRDT)

-- | Grow-only set
type GSet = Set

instance Ord a => CvRDT (Set a)
