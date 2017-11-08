{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GlobalTime
    ( Time (..)
    , newTime
    , runSystem
      -- * Process
    , Process
    , runProcess
    ) where

import           Control.Monad.State.Strict (State, evalState, state)
import           Numeric.Natural (Natural)

newtype Time = Time Natural
    deriving (Eq, Ord, Show)

newtype Process a = Process (State Time a)
    deriving Functor

newtype System a = System (State Time a)
    deriving (Applicative, Functor, Monad)

runProcess :: Process a -> System a
runProcess (Process s) = System s

runSystem :: System a -> a
runSystem (System s) = evalState s (Time 0)

newTime :: Process Time
newTime =
    Process $ state $ \(Time t) -> let t' = Time (t + 1) in (t', t')
