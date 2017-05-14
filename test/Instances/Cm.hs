{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Instances.Cm () where

import Data.DeriveTH   (derive, makeArbitrary)
import Test.QuickCheck (Arbitrary, arbitrary, choose)

import CRDT.PNCounter.Cm (PNCounter (..))

derive makeArbitrary ''PNCounter

deriving instance Show (PNCounter a)
