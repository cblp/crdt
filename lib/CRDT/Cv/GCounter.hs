module CRDT.Cv.GCounter
    ( GCounter (..)
    , initial
    , query
    -- * Operation
    , increment
    ) where

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.Semigroup (Semigroup ((<>)))
import           Data.Semilattice (Semilattice)

-- | Grow-only counter.
newtype GCounter a = GCounter (IntMap a)
    deriving (Eq, Show)

instance Ord a => Semigroup (GCounter a) where
    GCounter x <> GCounter y = GCounter $ IntMap.unionWith max x y

-- | See 'CvRDT'
instance Ord a => Semilattice (GCounter a)

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
