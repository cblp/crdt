{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module CRDT.Cm
    ( CmRDT (..)
    , Update (..)
    , concurrent
    , updateLocally
    ) where

import           Algebra.PartialOrd (PartialOrd (leq))
import           Data.Kind (Type)

data Update a = Update
    { atSource :: a -> a
    , downstream :: a -> a
    }

-- | TODO(cblp, 2017-09-29) import from lattices >= 1.6
comparable :: PartialOrd a => a -> a -> Bool
comparable a b = a `leq` b || b `leq` a

concurrent :: PartialOrd a => a -> a -> Bool
concurrent a b = not $ comparable a b

class (PartialOrd (Op a), Eq (Query a)) => CmRDT a where
    type Op a :: Type
    update :: Op a -> Update a
    type Query a :: Type
    query :: a -> Query a

updateLocally :: CmRDT a => Op a -> a -> a
updateLocally op = downstream . atSource
  where
    Update{atSource, downstream} = update op
