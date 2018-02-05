{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

module CRDT.LamportClock.Simulation
    (
    -- * Lamport clock simulation
      LamportClockSim
    , LamportClockSimT (..)
    , ProcessSim
    , ProcessSimT (..)
    , runLamportClockSim
    , runLamportClockSimT
    , runProcessSim
    , runProcessSimT
    ) where

import           Control.Monad.Reader (ReaderT, ask, runReaderT)
import           Control.Monad.RWS.Strict (RWST, evalRWS, evalRWST)
import           Control.Monad.State.Strict (MonadState, get, gets, modify, put,
                                             state)
import           Control.Monad.Trans (MonadTrans, lift)
import           Data.Functor.Identity (Identity)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           CRDT.LamportClock (Clock, LamportTime (LamportTime), LocalTime,
                                    Pid, Process, advance, getPid, getTime)

-- | Lamport clock simulation. Key is 'Pid'.
-- Non-present value is equivalent to (0, initial).
newtype LamportClockSimT s m a =
    LamportClockSim ((RWST s () (Map Pid (ProcessState s)) m) a)
    deriving (Applicative, Functor, Monad)

type LamportClockSim s = LamportClockSimT s Identity

instance MonadTrans (LamportClockSimT s) where
    lift = LamportClockSim . lift

data ProcessState s = ProcessState
    { time :: LocalTime
    , var  :: s
    }

-- | ProcessSim inside Lamport clock simulation.
newtype ProcessSimT s m a = ProcessSim (ReaderT Pid (LamportClockSimT s m) a)
    deriving (Applicative, Functor, Monad)

type ProcessSim s = ProcessSimT s Identity

instance MonadTrans (ProcessSimT s) where
    lift = ProcessSim . lift . lift

instance Monad m => Process (ProcessSimT s m) where
    getPid = ProcessSim ask

instance Monad m => Clock (ProcessSimT s m) where
    getTime = ProcessSim $ do
        pid <- ask
        time <- lift $ preIncrementTime pid
        pure $ LamportTime time pid

    advance time = ProcessSim $ do
        pid <- ask
        lift . LamportClockSim $ do
            initial <- ask
            modify $ Map.alter (Just . advancePS initial) pid
      where
        advancePS initial Nothing = ProcessState{time, var = initial}
        advancePS _       (Just ProcessState{time = current, var}) =
            ProcessState{time = max time current, var}

instance Monad m => MonadState s (ProcessSimT s m) where
    get = ProcessSim $ do
        pid <- ask
        lift . LamportClockSim $ do
            initial <- ask
            gets $ maybe initial var . Map.lookup pid
    put var = ProcessSim $ do
        pid <- ask
        lift . LamportClockSim . modify $ Map.alter (Just . putPS) pid
      where
        putPS Nothing                   = ProcessState{time = 0, var}
        putPS (Just ProcessState{time}) = ProcessState{time,     var}

runLamportClockSim :: s -> LamportClockSim s a -> a
runLamportClockSim initial (LamportClockSim action) =
    fst $ evalRWS action initial mempty

runLamportClockSimT :: Monad m => s -> LamportClockSimT s m a -> m a
runLamportClockSimT initial (LamportClockSim action) =
    fst <$> evalRWST action initial mempty

runProcessSim :: Pid -> ProcessSim s a -> LamportClockSim s a
runProcessSim pid (ProcessSim action) = runReaderT action pid

runProcessSimT :: Pid -> ProcessSimT s m a -> LamportClockSimT s m a
runProcessSimT pid (ProcessSim action) = runReaderT action pid

preIncrementTime :: Monad m => Pid -> LamportClockSimT s m LocalTime
preIncrementTime pid = LamportClockSim $ do
    initial <- ask
    state $ \pss -> let
        ps@ProcessState{time} =
            case Map.lookup pid pss of
                Nothing -> ProcessState{time = 1, var = initial}
                Just ProcessState{time = current, var} ->
                    ProcessState{time = succ current, var}
        in
        (time, Map.insert pid ps pss)
