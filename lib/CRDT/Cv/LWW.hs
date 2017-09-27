{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module CRDT.Cv.LWW
    ( LWW
    , initial
    , assign
    , query
    ) where

import           Control.Monad.Reader (MonadReader, ask)
import           Data.Semigroup (Semigroup, (<>))

import           CRDT.Cv (CvRDT)
import           CRDT.Timestamp (Timestamp)

-- | Last write wins.
data LWW a = LWW
    { timestamp :: !Timestamp
    , value     :: !a
    }
    deriving (Eq, Ord, Show)

-- | Merge by choosing more recent timestamp.
instance Ord a => Semigroup (LWW a) where
    (<>) = max

instance Ord a => CvRDT (LWW a)

-- | Initialize state
initial :: MonadReader Timestamp m => a -> m (LWW a)
initial value = do
    timestamp <- ask
    pure LWW{timestamp, value}

-- | Change state
assign :: (Ord a, MonadReader Timestamp m) => a -> LWW a -> m (LWW a)
assign value cur = do
    timestamp <- ask
    pure $ cur <> LWW{timestamp, value}

-- | Query state
query :: LWW a -> a
query = value
