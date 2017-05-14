{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Instances.Cm () where

import Data.DeriveTH   (derives, makeArbitrary)
import Test.QuickCheck (Arbitrary, arbitrary, choose)

import CRDT.LWW          (LWW (..))
import CRDT.PNCounter.Cm (PNCounter (..))

derives [makeArbitrary] [''PNCounter, ''LWW]

deriving instance Show (PNCounter a)

deriving instance Show a => Show (LWW a)
