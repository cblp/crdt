{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module CRDT.Cv.LWW
    ( LWW
    , initial
    , assign
    , query
    ) where

import           Data.Semigroup ((<>))
import           Lens.Micro ((<&>))

import           CRDT (MonadTimestamp, getTimestamp)
import           CRDT.Cv.LWW.Internal

-- | Initialize state
initial :: (Functor m, MonadTimestamp m) => a -> m (LWW a)
initial value = getTimestamp <&> \timestamp -> LWW{timestamp = timestamp, value}

-- | Change state
assign :: (Functor m, MonadTimestamp m) => a -> LWW a -> m (LWW a)
assign value cur = getTimestamp <&> \timestamp -> cur <> LWW{timestamp, value}

-- | Query state
query :: LWW a -> a
query = value
