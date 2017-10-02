{-# LANGUAGE TypeFamilies #-}

module Data.Observe
    ( Observe (..)
    ) where

import           Data.Kind (Type)

class Observe a where
    type Observed a :: Type
    observe :: a -> Observed a
