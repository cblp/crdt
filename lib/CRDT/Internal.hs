{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CRDT.Internal where

import           Control.Monad.State.Strict (State, modify)
import           Data.EnumMap.Strict (EnumMap)
import qualified Data.EnumMap.Strict as EnumMap
import           Data.Functor (($>))
import           Data.Maybe (fromMaybe)
import           Data.Word (Word32)

type Time = Word

-- | Unique process identifier
newtype Pid = Pid Word32
    deriving (Enum, Eq, Ord, Show)

-- | TODO(cblp, 2017-09-28) Use bounded-intmap
type PidClock = State (EnumMap Pid Time)

-- | Make sure all subsequent calls to 'getTimestamp' return timestamps
-- greater than all prior calls.
barrier :: [Pid] -> PidClock ()
barrier pids =
    modify $ \clocks -> let
        selectedClocks = EnumMap.fromList
            [(pid, fromMaybe 0 $ EnumMap.lookup pid clocks) | pid <- pids]
        in
        if null selectedClocks then
            clocks
        else
            EnumMap.union
                (selectedClocks $> succ (maximum selectedClocks))
                clocks
