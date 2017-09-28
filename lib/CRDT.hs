{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module CRDT
    ( Pid
    , Timestamp
    , MonadTimestamp (..)
    -- * Pid clock
    , PidClock
    , runPidClock
    , runProcess
    ) where

import           Control.Monad.Reader (ReaderT, ask, runReaderT)
import           Control.Monad.State.Strict (evalState)
import           Lens.Micro (at, non)
import           Lens.Micro.Extra ((<<+=))

import           CRDT.Internal

-- | Timestamps are assumed unique, totally ordered,
-- and consistent with causal order;
-- i.e., if assignment 1 happened-before assignment 2,
-- the former’s timestamp is less than the latter’s.
data Timestamp = Timestamp !Time !Pid
    deriving (Eq, Ord, Show)

class MonadTimestamp m where
    getTimestamp :: m Timestamp

type Process = ReaderT Pid PidClock

instance MonadTimestamp Process where
    getTimestamp = do
        pid <- ask
        time <- at pid . non 0 <<+= 1
        pure $ Timestamp time pid

runPidClock :: PidClock a -> a
runPidClock action = evalState action mempty

runProcess :: Pid -> Process a -> PidClock a
runProcess pid action = runReaderT action pid
