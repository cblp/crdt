{-# OPTIONS_GHC -fno-warn-orphans #-}

module Compat where

import           Prelude hiding (fail)

import           Control.Monad.Fail (MonadFail, fail)
import           Control.Monad.Reader (ReaderT, lift)

instance MonadFail m => MonadFail (ReaderT r m) where
    fail = lift . fail
