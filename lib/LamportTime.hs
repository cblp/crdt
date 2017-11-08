{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module LamportTime
    ( Pid (..)
    -- * Lamport's timestamp (for a single process)
    , Time (..)
    , newTime
    -- * Lamport's clock (for a whole multi-process system)
    , System
    , runSystem
    -- * Lamport's process
    , Process
    , runProcess
    ) where

import           Control.Monad.Reader (ReaderT, ask, runReaderT)
import           Control.Monad.State.Strict (MonadState, State, evalState,
                                             state)
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Numeric.Natural (Natural)

newtype Time = Time Natural
    deriving (Eq, Ord, Show)

-- | Unique process identifier
newtype Pid = Pid Int
    deriving (Eq, Ord, Show)

-- | Key is 'Pid'. Non-present value is equivalent to 0.
-- TODO(cblp, 2017-09-28) Use bounded-intmap
type SystemState = IntMap Time

type System = State SystemState

type Process = ReaderT Pid System

newTime :: Process Time
newTime = ask >>= preIncrementAt

runSystem :: System a -> a
runSystem action = evalState action mempty

runProcess :: Pid -> Process a -> System a
runProcess pid action = runReaderT action pid

preIncrementAt :: MonadState SystemState m => Pid -> m Time
preIncrementAt (Pid pid) = state $ \m -> let
    lt' = case IntMap.lookup pid m of
        Nothing -> Time 1
        Just (Time t) -> Time (t + 1)
    in (lt', IntMap.insert pid lt' m)
