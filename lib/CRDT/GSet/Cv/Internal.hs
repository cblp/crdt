{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CRDT.GSet.Cv.Internal where

import Data.Set       (Set)
import Data.Semigroup (Semigroup)

import CRDT.Cv        (CvRDT)

-- | Grow-only set
newtype GSet a = GSet (Set a)
  deriving (Eq, Ord, Semigroup, Show)

instance Ord a => CvRDT (GSet a)
