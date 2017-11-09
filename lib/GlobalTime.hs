{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GlobalTime
    ( GlobalTime (..)
    , newTime
    , runSystem
      -- * Process
    , Process
    , runProcess
    ) where

import           Control.Monad.State.Strict (State, evalState, state)
import           Numeric.Natural (Natural)

newtype GlobalTime = GlobalTime Natural
    deriving (Eq, Ord, Show)

newtype Process a = Process (State GlobalTime a)
    deriving Functor

newtype System a = System (State GlobalTime a)
    deriving (Applicative, Functor, Monad)

runProcess :: Process a -> System a
runProcess (Process s) = System s

runSystem :: System a -> a
runSystem (System s) = evalState s (GlobalTime 0)

newTime :: Process GlobalTime
newTime =
    Process $ state $ \(GlobalTime t) -> let t' = GlobalTime (t + 1) in (t', t')
