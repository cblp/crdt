{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module CRDT.Cm.ORSet
    ( ORSet (..)
    , Intent (..)
    , Payload (..)
    , Tag (..)
    , query
    ) where

import           Data.MultiMap (MultiMap)
import qualified Data.MultiMap as MultiMap
import           Data.Set (Set)
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

    initial = Payload{elements = MultiMap.empty, version = 0}

    makeOp (Add a) Payload{version} = Just $ do
        pid <- getPid
        pure $ OpAdd a $ Tag pid version
    makeOp (Remove a) Payload{elements} =
        Just . pure . OpRemove a $ MultiMap.lookup a elements

    apply op Payload{elements, version} = Payload
        { version  = version + 1
        , elements = case op of
            OpAdd    a tag  -> MultiMap.insert     a tag  elements
            OpRemove a tags -> MultiMap.deleteMany a tags elements
        }

query :: (Ord a, Foldable f) => f (ORSet a) -> Set a
query = MultiMap.keysSet . elements . Cm.query
