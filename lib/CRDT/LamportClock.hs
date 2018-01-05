{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CRDT.LamportClock
    ( Pid (..)
    -- * Lamport timestamp (for a single process)
    , LamportTime (..)
    , getRealLamportTime
    , getTime
    , advance
    -- * Lamport clock (for a whole multi-process system)
    , LamportClock (..)
    , runLamportClock
    -- * Process
    , Process (..)
    , getPid
    , runProcess
    ) where

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

-- | Key is 'Pid'. Non-present value is equivalent to 0.
newtype LamportClock a = LamportClock (State (Map Pid LocalTime) a)
    deriving (Applicative, Functor, Monad)

newtype Process a = Process (ReaderT Pid LamportClock a)
    deriving (Applicative, Functor, Monad)

getPid :: Process Pid
getPid = Process ask

getTime :: Process LamportTime
getTime = Process $ do
    pid <- ask
    time <- lift $ preIncrementAt pid
    pure $ LamportTime time pid

runLamportClock :: LamportClock a -> a
runLamportClock (LamportClock action) = evalState action mempty

runProcess :: Pid -> Process a -> LamportClock a
runProcess pid (Process action) = runReaderT action pid

preIncrementAt :: Pid -> LamportClock LocalTime
preIncrementAt pid =
    LamportClock . state $ \m -> let
        lt' = succ . fromMaybe 0 $ Map.lookup pid m
        in (lt', Map.insert pid lt' m)

advance :: LamportTime -> Process ()
advance (LamportTime time _) = Process $ do
    pid <- ask
    lift . LamportClock . modify $ Map.insertWith max pid time

getRealLocalTime :: IO LocalTime
getRealLocalTime = round . utcTimeToPOSIXSeconds <$> getCurrentTime

-- TODO(cblp, 2018-01-05) monotonic
getRealLamportTime :: IO LamportTime
getRealLamportTime =
    LamportTime <$> getRealLocalTime <*> (Pid . decodeMac <$> getMac)
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
