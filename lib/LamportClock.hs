{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module LamportClock
    ( Pid
    , Timestamp
    , Clock (..)
    -- * Lamport clock
    , LamportClock
    , runLamportClock
    , runProcess
    ) where

import           Control.Monad.Reader (ReaderT, ask, runReaderT)
import           Control.Monad.State.Strict (evalState)
import           Lens.Micro (at, non)
import           Lens.Micro.Extra ((<<+=))

import           LamportClock.Internal (LamportClock, Pid, Time)

-- | Timestamps are assumed unique, totally ordered,
-- and consistent with causal order;
-- i.e., if assignment 1 happened-before assignment 2,
-- the former’s timestamp is less than the latter’s.
data Timestamp = Timestamp !Time !Pid
    deriving (Eq, Ord, Show)

class Functor f => Clock f where
    -- | Get another unique timestamp
    newTimestamp :: f Timestamp

type Process = ReaderT Pid LamportClock

instance Clock Process where
    newTimestamp = do
        pid <- ask
        time <- at pid . non 0 <<+= 1
        pure $ Timestamp time pid

runLamportClock :: LamportClock a -> a
runLamportClock action = evalState action mempty

runProcess :: Pid -> Process a -> LamportClock a
runProcess pid action = runReaderT action pid
