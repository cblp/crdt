{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CRDT.LamportClock.Simulation
    (
    -- * Lamport clock simulation
      LamportClockSim
    , LamportClockSimT (..)
    , ObservedTime (..)
    , ProcessSim
    , ProcessSimT (..)
    , evalProcessSim
    , runLamportClockSim
    , runLamportClockSimT
    , runProcessSim
    , runProcessSimT
    ) where

import           Control.Monad.Except (ExceptT, MonadError, runExceptT,
                                       throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (ask)
import           Control.Monad.RWS.Strict (RWST, evalRWST, tell)
import           Control.Monad.State.Strict (StateT, evalState, evalStateT,
                                             modify, state)
import           Control.Monad.Trans (MonadTrans, lift)
import           Data.Bifunctor (second)
import           Data.Foldable (toList)
import           Data.Functor.Identity (Identity)
import           Data.Hashable (hash)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Numeric.Natural (Natural)

import           CRDT.LamportClock (Clock, LamportTime (LamportTime), LocalTime,
                                    Pid (Pid), Process, advance, getPid,
                                    getTimes)

#if __GLASGOW_HASKELL__ < 800
import           Compat ()
#endif /* __GLASGOW_HASKELL__ < 800 */

-- | Lamport clock simulation. Key is 'Pid'.
-- Non-present value is equivalent to (0, initial).
newtype LamportClockSimT m a =
    LamportClockSim (ExceptT String (StateT (Map Pid LocalTime) m) a)
    deriving (Applicative, Functor, Monad, MonadError String)

instance MonadTrans LamportClockSimT where
    lift = LamportClockSim . lift . lift

instance Monad m => MonadFail (LamportClockSimT m) where
    fail = throwError

instance MonadIO m => MonadIO (LamportClockSimT m) where
    liftIO io = LamportClockSim $ liftIO io

type LamportClockSim = LamportClockSimT Identity

data ObservedTime = ObservedTime{stamp :: LocalTime, count :: Natural}

-- | ProcessSim inside Lamport clock simulation.
newtype ProcessSimT m a =
    ProcessSim (RWST Pid (Seq ObservedTime) () (LamportClockSimT m) a)
    deriving (Applicative, Functor, Monad, MonadFail)

type ProcessSim = ProcessSimT Identity

instance MonadTrans ProcessSimT where
    lift = ProcessSim . lift . lift

instance Monad m => Process (ProcessSimT m) where
    getPid = ProcessSim ask

instance Monad m => Clock (ProcessSimT m) where
    getTimes n' = ProcessSim $ do
        pid <- ask
        time <- lift $ preIncreaseTime n pid
        tell $ Seq.singleton ObservedTime{stamp = time, count = n}
        pure $ LamportTime time pid
      where
        n = max n' 1

    advance time = ProcessSim $ do
        pid <- ask
        lift . LamportClockSim . modify $ Map.alter (Just . advancePS) pid
      where
        advancePS = \case
            Nothing      -> time
            Just current -> max time current

instance MonadIO m => MonadIO (ProcessSimT m) where
    liftIO io = ProcessSim $ liftIO io

runLamportClockSim :: LamportClockSim a -> Either String a
runLamportClockSim (LamportClockSim action) =
    evalState (runExceptT action) mempty

runLamportClockSimT :: Monad m => LamportClockSimT m a -> m (Either String a)
runLamportClockSimT (LamportClockSim action) =
    evalStateT (runExceptT action) mempty

runProcessSim :: Pid -> ProcessSim a -> LamportClockSim a
runProcessSim = runProcessSimT

runProcessSimT :: Monad m => Pid -> ProcessSimT m a -> LamportClockSimT m a
runProcessSimT pid (ProcessSim action) = fst <$> evalRWST action pid ()

evalProcessSim :: Pid -> ProcessSim a -> LamportClockSim (a, [ObservedTime])
evalProcessSim pid (ProcessSim action) =
    second toList <$> evalRWST action pid ()

-- | Increase time by pid and return new value
preIncreaseTime :: Monad m => Natural -> Pid -> LamportClockSimT m LocalTime
preIncreaseTime n pid = LamportClockSim $ state $ \pss ->
    let time0 = fromMaybe 0 $ Map.lookup pid pss
        Pid p = pid
        d     = fromIntegral . abs $ hash (time0, n, p)
        time  = time0 + max 1 d
    in  (time, Map.insert pid time pss)
