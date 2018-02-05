module Data.MultiMap
    ( MultiMap (..)
    , assocs
    , delete
    , deleteMany
    , empty
    , insert
    , keysSet
    , lookup
    , singleton
    ) where

import           Prelude hiding (lookup)

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Semigroup ((<>))
import           Data.Set (Set)
import qualified Data.Set as Set

newtype MultiMap k v = MultiMap (Map k (Set v))
    deriving (Eq, Show)

assocs :: MultiMap k v -> [(k, [v])]
assocs (MultiMap m) = Map.assocs $ Set.toList <$> m

delete :: (Ord k, Ord v) => k -> v -> MultiMap k v -> MultiMap k v
delete k v (MultiMap m) = MultiMap $ Map.update delete' k m
  where
    delete' s = let s' = Set.delete v s in if null s' then Nothing else Just s'

deleteMany ::
    (Ord k, Ord v) => k -> Set v -> MultiMap k v -> MultiMap k v
deleteMany k vs (MultiMap m) = MultiMap $ Map.update deleteMany' k m
  where
    deleteMany' s =
        let s' = Set.difference s vs in if null s' then Nothing else Just s'

empty :: MultiMap k v
empty = MultiMap Map.empty

insert :: (Ord k, Ord v) => k -> v -> MultiMap k v -> MultiMap k v
insert k v (MultiMap m) =
    MultiMap $ Map.insertWith (<>) k (Set.singleton v) m

keysSet :: MultiMap k v -> Set k
keysSet (MultiMap m) = Map.keysSet m

-- | If no key in the map then the result is empty.
lookup :: Ord k => k -> MultiMap k v -> Set v
lookup k (MultiMap m) = fromMaybe Set.empty $ Map.lookup k m

singleton :: k -> v -> MultiMap k v
singleton k v = MultiMap $ Map.singleton k $ Set.singleton v
