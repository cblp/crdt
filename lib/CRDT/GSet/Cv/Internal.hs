{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module CRDT.GSet.Cv.Internal where

import Data.Set (Set)

import CRDT.Cv  (CvRDT)

-- | Grow-only set
type GSet = Set

instance Ord a => CvRDT (Set a)
