{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CRDT.LamportClock
    ( Pid (..)
    -- * Lamport timestamp (for a single process)
    , LamportTime (..)
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
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.Maybe (fromMaybe)
import           Numeric.Natural (Natural)

type LocalTime = Natural

data LamportTime = LamportTime !LocalTime !Pid
    deriving (Eq, Ord, Show)

-- | Unique process identifier
newtype Pid = Pid Int
    deriving (Eq, Ord, Show)

-- | Key is 'Pid'. Non-present value is equivalent to 0.
newtype LamportClock a = LamportClock (State (IntMap LocalTime) a)
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
preIncrementAt (Pid pid) =
    LamportClock . state $ \m -> let
        lt' = succ . fromMaybe 0 $ IntMap.lookup pid m
        in (lt', IntMap.insert pid lt' m)

advance :: LamportTime -> Process ()
advance (LamportTime time _) = Process $ do
    Pid pid <- ask
    lift . LamportClock . modify $ IntMap.insertWith max pid time
