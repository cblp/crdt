{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
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

import           Control.Monad.Except (ExceptT, MonadError, runExceptT,
                                       throwError)
import           Control.Monad.Fail (MonadFail, fail)
import           Control.Monad.Reader (ReaderT, ask, runReaderT)
import           Control.Monad.State.Strict (StateT, evalState, evalStateT,
                                             modify, state)
import           Control.Monad.Trans (MonadTrans, lift)
import           Data.Functor.Identity (Identity)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           CRDT.LamportClock (Clock, LamportTime (LamportTime), LocalTime,
                                    Pid, Process, advance, getPid, getTime)

-- | Lamport clock simulation. Key is 'Pid'.
-- Non-present value is equivalent to (0, initial).
newtype LamportClockSimT m a =
    LamportClockSim (ExceptT String (StateT (Map Pid LocalTime) m) a)
    deriving (Applicative, Functor, Monad, MonadError String)

instance MonadTrans LamportClockSimT where
    lift = LamportClockSim . lift . lift

instance Monad m => MonadFail (LamportClockSimT m) where
    fail = throwError

type LamportClockSim = LamportClockSimT Identity

-- | ProcessSim inside Lamport clock simulation.
newtype ProcessSimT m a = ProcessSim (ReaderT Pid (LamportClockSimT m) a)
    deriving (Applicative, Functor, Monad, MonadFail)

type ProcessSim = ProcessSimT Identity

instance MonadTrans ProcessSimT where
    lift = ProcessSim . lift . lift

instance Monad m => Process (ProcessSimT m) where
    getPid = ProcessSim ask

instance Monad m => Clock (ProcessSimT m) where
    getTime = ProcessSim $ do
        pid <- ask
        time <- lift $ preIncrementTime pid
        pure $ LamportTime time pid

    advance time = ProcessSim $ do
        pid <- ask
        lift . LamportClockSim . modify $ Map.alter (Just . advancePS) pid
      where
        advancePS = \case
            Nothing      -> time
            Just current -> max time current

runLamportClockSim :: LamportClockSim a -> Either String a
runLamportClockSim (LamportClockSim action) =
    evalState (runExceptT action) mempty

runLamportClockSimT :: Monad m => LamportClockSimT m a -> m (Either String a)
runLamportClockSimT (LamportClockSim action) =
    evalStateT (runExceptT action) mempty

runProcessSim :: Pid -> ProcessSim a -> LamportClockSim a
runProcessSim pid (ProcessSim action) = runReaderT action pid

runProcessSimT :: Pid -> ProcessSimT m a -> LamportClockSimT m a
runProcessSimT pid (ProcessSim action) = runReaderT action pid

preIncrementTime :: Monad m => Pid -> LamportClockSimT m LocalTime
preIncrementTime pid = LamportClockSim $ state $ \pss -> let
    time = case Map.lookup pid pss of
        Nothing      -> 1
        Just current -> succ current
    in
    (time, Map.insert pid time pss)
