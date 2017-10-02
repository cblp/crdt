{-# OPTIONS_GHC -Wno-orphans #-} -- TODO(cblp, 2017-10-02) join with Cv.LWW

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module CRDT.Cm.LWW
    ( LWW (..)
    , Assign (..)
    ) where

import           Algebra.PartialOrd (PartialOrd (..))
import           Data.Observe (Observe (..))
import           Data.Semigroup ((<>))
import           Lens.Micro ((<&>))

import           CRDT.Cm (CmRDT (..))
import           CRDT.Cv.LWW (LWW (..))
import           LamportClock (Clock (newTimestamp))

instance PartialOrd (LWW a) where
    leq _ _ = False

newtype Assign a = Assign a
    deriving (Eq, Show)

instance Eq a => CmRDT (LWW a) (Assign a) (LWW a) where
    updateAtSource (Assign value) =
        newTimestamp <&> \t -> LWW{timestamp = t, value}
    updateDownstream = (<>)

instance Observe (LWW a) where
    type Observed (LWW a) = a
    observe = value
