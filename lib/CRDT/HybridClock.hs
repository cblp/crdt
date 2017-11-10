{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CRDT.HybridClock
    ( Pid (..)
    -- * Hybrid timestamp (for a single process)
    , HybridTime (..)
    , getTime
    , advance
    -- * Hybrid clock (for a whole multi-process system)
    , HybridClock (..)
    , runHybridClock
    -- * Process
    , Process (..)
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

data HybridTime = HybridTime !LocalTime !Pid
    deriving (Eq, Ord, Show)

-- | Unique process identifier
newtype Pid = Pid Int
    deriving (Eq, Ord, Show)

-- | Key is 'Pid'. Non-present value is equivalent to 0.
-- TODO(cblp, 2017-09-28) Use bounded-intmap
newtype HybridClock a = HybridClock (State (IntMap LocalTime) a)
    deriving (Applicative, Functor, Monad)

newtype Process a = Process (ReaderT Pid HybridClock a)
    deriving (Applicative, Functor, Monad)

getTime :: Process HybridTime
getTime = Process $ do
    pid <- ask
    time <- lift $ preIncrementAt pid
    pure $ HybridTime time pid

runHybridClock :: HybridClock a -> a
runHybridClock (HybridClock action) = evalState action mempty

runProcess :: Pid -> Process a -> HybridClock a
runProcess pid (Process action) = runReaderT action pid

preIncrementAt :: Pid -> HybridClock LocalTime
preIncrementAt (Pid pid) =
    HybridClock . state $ \m -> let
        lt' = succ . fromMaybe 0 $ IntMap.lookup pid m
        in (lt', IntMap.insert pid lt' m)

advance :: HybridTime -> Process ()
advance (HybridTime time _) = Process $ do
    Pid pid <- ask
    lift . HybridClock . modify $ IntMap.insertWith max pid time
