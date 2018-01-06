{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CRDT.LamportClock
    ( Pid (..)
    -- * Lamport timestamp (for a single process)
    , Clock (..)
    , LamportTime (..)
    , LocalTime
    , Process (..)
    -- * Lamport clock simulation
    , LamportClockSim (..)
    , ProcessSim (..)
    , runLamportClockSim
    , runProcessSim
    -- * Real Lamport clock
    , LamportClock
    , runLamportClock
    -- * Helpers
    , getRealLocalTime
    ) where

import           Control.Concurrent.STM (TVar, atomically, modifyTVar',
                                         readTVar, writeTVar)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (ReaderT, ask, runReaderT)
import           Control.Monad.State.Strict (State, evalState, modify, state)
import           Control.Monad.Trans (lift)
import           Data.Binary (decode)
import qualified Data.ByteString.Lazy as BSL
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Time (getCurrentTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           Data.Word (Word64)
import           Network.Info (MAC (MAC), getNetworkInterfaces, mac)
import           Numeric (showHex)
import           Numeric.Natural (Natural)
import           Safe (headDef)

type LocalTime = Natural

data LamportTime = LamportTime !LocalTime !Pid
    deriving (Eq, Ord)

instance Show LamportTime where
    show (LamportTime time (Pid pid)) = showHex time "" ++ '-' : showHex pid ""

-- | Unique process identifier
newtype Pid = Pid Word64
    deriving (Eq, Ord, Show)

-- | Lamport clock simpulation. Key is 'Pid'.
-- Non-present value is equivalent to 0.
newtype LamportClockSim a = LamportClockSim (State (Map Pid LocalTime) a)
    deriving (Applicative, Functor, Monad)

-- | ProcessSim inside Lamport clock simpulation.
newtype ProcessSim a = ProcessSim (ReaderT Pid LamportClockSim a)
    deriving (Applicative, Functor, Monad)

class Monad m => Process m where
    getPid :: m Pid

runLamportClockSim :: LamportClockSim a -> a
runLamportClockSim (LamportClockSim action) = evalState action mempty

runProcessSim :: Pid -> ProcessSim a -> LamportClockSim a
runProcessSim pid (ProcessSim action) = runReaderT action pid

preIncrementAt :: Pid -> LamportClockSim LocalTime
preIncrementAt pid =
    LamportClockSim . state $ \m -> let
        lt' = succ . fromMaybe 0 $ Map.lookup pid m
        in (lt', Map.insert pid lt' m)

getRealLocalTime :: IO LocalTime
getRealLocalTime = round . utcTimeToPOSIXSeconds <$> getCurrentTime

getPidByMac :: IO Pid
getPidByMac = Pid . decodeMac <$> getMac
  where
    getMac :: IO MAC
    getMac =
        headDef (error "Can't get any non-zero MAC address of this machine")
        . filter (/= minBound)
        . map mac
        <$> getNetworkInterfaces

    decodeMac :: MAC -> Word64
    decodeMac (MAC b5 b4 b3 b2 b1 b0) =
        decode $ BSL.pack [0, 0, b5, b4, b3, b2, b1, b0]

class Process m => Clock m where
    getTime :: m LamportTime
    advance :: LocalTime -> m ()

instance Process ProcessSim where
    getPid = ProcessSim ask

instance Clock ProcessSim where
    getTime = ProcessSim $ do
        pid <- ask
        time <- lift $ preIncrementAt pid
        pure $ LamportTime time pid

    advance time = ProcessSim $ do
        pid <- ask
        lift . LamportClockSim . modify $ Map.insertWith max pid time

-- TODO(cblp, 2018-01-06) benchmark and compare with 'atomicModifyIORef'
newtype LamportClock a = LamportClock (ReaderT (TVar LocalTime) IO a)
    deriving (Applicative, Functor, Monad, MonadIO)

runLamportClock :: TVar LocalTime -> LamportClock a -> IO a
runLamportClock var (LamportClock action) = runReaderT action var

instance Process LamportClock where
    getPid = liftIO getPidByMac

instance Clock LamportClock where
    advance time = LamportClock $ do
        timeVar <- ask
        lift $ atomically $ modifyTVar' timeVar $ max time

    getTime = LamportClock $ do
        timeVar <- ask
        lift $ do
            realTime <- getRealLocalTime
            time1 <- atomically $ do
                time0 <- readTVar timeVar
                let time1 = max realTime (time0 + 1)
                writeTVar timeVar time1
                pure time1
            pid <- getPidByMac
            pure $ LamportTime time1 pid
