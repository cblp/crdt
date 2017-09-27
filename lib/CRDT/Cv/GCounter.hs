module CRDT.Cv.GCounter
    ( GCounter
    , initial
    , query
    -- * Operation
    , increment
    ) where

import qualified Data.IntMap.Strict as IntMap

import           CRDT.Cv.GCounter.Internal

-- | Increment counter
increment
    :: Num a
    => Word -- ^ replica id
    -> GCounter a
    -> GCounter a
increment replicaId (GCounter imap) = GCounter (IntMap.insertWith (+) i 1 imap)
  where
    i = fromIntegral replicaId

-- | Initial state
initial :: GCounter a
initial = GCounter IntMap.empty

-- | Get value from the state
query :: Num a => GCounter a -> a
query (GCounter v) = mapSum v
  where
    mapSum = IntMap.foldr (+) 0
