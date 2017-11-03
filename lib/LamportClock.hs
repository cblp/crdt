{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module LamportClock
    ( Pid (..)
    , Time
    -- * Lamport's timestamp (for a single process)
    , Timestamp (..)
    , newTimestamp
    -- * Lamport's clock (for a whole multi-process system)
    , LamportClock
    , runLamportClock
    -- * Lamport's process
    , Process
    , runProcess
    -- * Debug tool
    , barrier
    ) where

import           Control.Arrow (first)
import           Control.Monad.Reader (ReaderT, ask, runReaderT)
import           Control.Monad.State.Strict (MonadState, State, evalState,
                                             modify, state)
import           Data.Functor (($>))
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.Maybe (fromMaybe)

type Time = Word

-- | Unique process identifier
newtype Pid = Pid Int
    deriving (Eq, Ord, Show)

unPid :: Pid -> Int
unPid (Pid pid) = pid

-- | Key is 'Pid'. Non-present value is equivalent to 0.
-- TODO(cblp, 2017-09-28) Use bounded-intmap
type LamportTime = IntMap Time

type LamportClock = State LamportTime

-- | XXX Make sure all subsequent calls to 'newTimestamp' return timestamps
-- greater than all prior calls.
barrier :: [Pid] -> LamportClock ()
barrier pids =
    modify $ \clocks -> let
        selectedClocks = lamportTimeFromList
            [(pid, fromMaybe 0 $ lamportTimeLookup pid clocks) | pid <- pids]
        in
        if null selectedClocks then
            clocks
        else
            IntMap.union
                (selectedClocks $> succ (maximum selectedClocks))
                clocks

-- | Timestamps are assumed unique, totally ordered,
-- and consistent with causal order;
-- i.e., if assignment 1 happened-before assignment 2,
-- the former’s timestamp is less than the latter’s.
data Timestamp = Timestamp !Time !Pid
    deriving (Eq, Ord, Show)

type Process = ReaderT Pid LamportClock

newTimestamp :: Process Timestamp
newTimestamp = do
    pid <- ask
    time <- postIncrementAt pid
    pure $ Timestamp time pid

runLamportClock :: LamportClock a -> a
runLamportClock action = evalState action mempty

runProcess :: Pid -> Process a -> LamportClock a
runProcess pid action = runReaderT action pid

postIncrementAt :: MonadState LamportTime m => Pid -> m Time
postIncrementAt (Pid pid) = state $ \m ->
    let v = fromMaybe 0 $ IntMap.lookup pid m
    in  (v, IntMap.insert pid (v + 1) m)

lamportTimeFromList :: [(Pid, Time)] -> LamportTime
lamportTimeFromList = IntMap.fromList . map (first unPid)

lamportTimeLookup :: Pid -> LamportTime -> Maybe Time
lamportTimeLookup = IntMap.lookup . unPid
