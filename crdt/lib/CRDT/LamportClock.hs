{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module CRDT.LamportClock
    ( Pid (..)
    -- * Lamport timestamp (for a single process)
    , Clock (..)
    , LamportTime (..)
    , getTime
    , LocalTime
    , Process (..)
    -- * Real Lamport clock
    , LamportClock
    , runLamportClock
    -- * Helpers
    , getRealLocalTime
    , getMacAddress
    ) where

import           Control.Concurrent.STM (TVar, atomically, modifyTVar',
                                         readTVar, writeTVar)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (ReaderT, ask, runReaderT)
import           Control.Monad.State.Strict (StateT)
import           Control.Monad.Trans (lift)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Word (Word64)
import           Numeric.Natural (Natural)

import           MacAddress (getMacAddress)

-- | Unix time in 10^{-7} seconds (100 ns), as in RFC 4122 and Swarm RON.
type LocalTime = Natural

data LamportTime = LamportTime LocalTime Pid
    deriving (Eq, Ord)

instance Show LamportTime where
    show (LamportTime time (Pid pid)) = show time ++ '.' : show pid

-- | Unique process identifier
newtype Pid = Pid Word64
    deriving (Eq, Ord, Show)

class Monad m => Process m where
    getPid :: m Pid

getRealLocalTime :: IO LocalTime
getRealLocalTime = round . (* 10000000) <$> getPOSIXTime

class Process m => Clock m where
    -- | Get sequential timestamps.
    --
    -- Laws:
    --    1.  t1 <- getTimes n
    --        t2 <- getTime
    --        t2 >= t1 + n
    --
    --    2. getTimes 0 == getTimes 1
    getTimes
        :: Natural -- ^ number of needed timestamps
        -> m LamportTime
        -- ^ Starting value of the range.
        -- So return value @t@ means range @[t .. t + n - 1]@.
    advance :: LocalTime -> m ()

getTime :: Clock m => m LamportTime
getTime = getTimes 1

-- TODO(cblp, 2018-01-06) benchmark and compare with 'atomicModifyIORef'
newtype LamportClock a = LamportClock (ReaderT (TVar LocalTime) IO a)
    deriving (Applicative, Functor, Monad, MonadIO)

runLamportClock :: TVar LocalTime -> LamportClock a -> IO a
runLamportClock var (LamportClock action) = runReaderT action var

instance Process LamportClock where
    getPid = Pid <$> liftIO getMacAddress

instance Clock LamportClock where
    advance time = LamportClock $ do
        timeVar <- ask
        lift $ atomically $ modifyTVar' timeVar $ max time

    getTimes n' = LamportTime <$> getTimes' <*> getPid
      where
        n = max n' 1
        getTimes' = LamportClock $ do
            timeVar <- ask
            lift $ do
                realTime <- getRealLocalTime
                atomically $ do
                    timeCur <- readTVar timeVar
                    let timeRangeStart = max realTime (timeCur + 1)
                    writeTVar timeVar $ timeRangeStart + n - 1
                    pure timeRangeStart

instance Process m => Process (ReaderT r m) where
    getPid = lift getPid

instance Process m => Process (StateT s m) where
    getPid = lift getPid

instance Clock m => Clock (ReaderT r m) where
    advance = lift . advance
    getTimes = lift . getTimes

instance Clock m => Clock (StateT s m) where
    advance = lift . advance
    getTimes = lift . getTimes
