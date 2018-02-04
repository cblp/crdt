{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module CRDT.Cm.ORSet
    (
    -- * OR-Set
      ORSet (..)
    , Intent (..)
    , Payload (..)
    , Tag (..)
    , initial
    , query
    -- * MultiMap
    , MultiMap (..)
    , multiMapAssocs
    ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Semigroup ((<>))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Numeric.Natural (Natural)

import           CRDT.Cm (CausalOrd, CmRDT)
import qualified CRDT.Cm as Cm
import           CRDT.LamportClock (Pid (Pid), getPid)

data ORSet a = OpAdd a Tag | OpRemove a (Set Tag)
    deriving Show

data Intent a = Add a | Remove a
    deriving Show

data Payload a = Payload
    { elements :: MultiMap a Tag
    , version  :: Version
    }
    deriving (Eq, Show)

data Tag = Tag Pid Version
    deriving (Eq, Ord)

type Version = Natural

instance Show Tag where
    show (Tag (Pid pid) version) = show pid ++ '-' : show version

instance CausalOrd (ORSet a) where
    precedes _ _ = False

instance Ord a => CmRDT (ORSet a) where
    type Intent  (ORSet a) = Intent  a
    type Payload (ORSet a) = Payload a

    makeOp (Add a) Payload{version} = Just $ do
        pid <- getPid
        pure $ OpAdd a $ Tag pid version
    makeOp (Remove a) Payload{elements} =
        Just . pure . OpRemove a $ multiMapLookup a elements

    apply op Payload{elements, version} = Payload
        { version  = version + 1
        , elements = case op of
            OpAdd    a tag  -> multiMapInsert     a tag  elements
            OpRemove a tags -> multiMapDeleteMany a tags elements
        }

query :: (Ord a, Foldable f) => f (ORSet a) -> Set a
query = multiMapKeysSet . elements . Cm.query initial

initial :: Payload a
initial = Payload{elements = multiMapEmpty, version = 0}

-- MultiMap --------------------------------------------------------------------

newtype MultiMap k v = MultiMap (Map k (Set v))
    deriving (Eq, Show)

multiMapAssocs :: MultiMap k v -> [(k, [v])]
multiMapAssocs (MultiMap m) = Map.assocs $ Set.toList <$> m

multiMapDeleteMany ::
    (Ord k, Ord v) => k -> Set v -> MultiMap k v -> MultiMap k v
multiMapDeleteMany k vs (MultiMap m) = MultiMap $ Map.update deleteMany k m
  where
    deleteMany s =
        let s' = Set.difference s vs in if null s' then Nothing else Just s'

multiMapEmpty :: MultiMap k v
multiMapEmpty = MultiMap Map.empty

multiMapInsert :: (Ord k, Ord v) => k -> v -> MultiMap k v -> MultiMap k v
multiMapInsert k v (MultiMap m) =
    MultiMap $ Map.insertWith (<>) k (Set.singleton v) m

multiMapKeysSet :: MultiMap k v -> Set k
multiMapKeysSet (MultiMap m) = Map.keysSet m

-- | If no key in the map then the result is empty.
multiMapLookup :: Ord k => k -> MultiMap k v -> Set v
multiMapLookup k (MultiMap m) = fromMaybe Set.empty $ Map.lookup k m
