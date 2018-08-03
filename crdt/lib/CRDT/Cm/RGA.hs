{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

-- | Replicated Growable Array (RGA)
module CRDT.Cm.RGA
    ( RGA (..)
    , RgaIntent (..)
    , RgaPayload (..)
    , fromString
    , load
    , toString
    , toVector
    ) where

import           Prelude hiding (lookup)

import           Control.Monad.Fail (MonadFail)
import           Control.Monad.State.Strict (MonadState)
import           Data.Empty (AsEmpty (..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Semigroup ((<>))
import           Data.Vector (Vector, (//))
import qualified Data.Vector as Vector

import           CRDT.Cm (CausalOrd, CmRDT, Intent, Payload, apply, initial,
                          makeAndApplyOp, makeOp, precedes)
import           CRDT.LamportClock (Clock, LamportTime (LamportTime), advance,
                                    getTime)

-- | Using 'LamportTime' as an identifier for vertices
type VertexId = LamportTime

data RgaPayload a = RgaPayload
    { vertices  :: Vector (VertexId, a) -- TODO(cblp, 2018-02-06) Unbox
    , vertexIxs :: Map VertexId Int
      -- ^ indices in `vertices` vector
    }
    deriving (Eq, Show)

-- | Is added and is not removed.
lookup :: AsEmpty a => VertexId -> RgaPayload a -> Bool
lookup v RgaPayload { vertices, vertexIxs } = case Map.lookup v vertexIxs of
    Just ix -> let (_, a) = vertices Vector.! ix in isNotEmpty a
    Nothing -> False

data RgaIntent a
    = AddAfter (Maybe VertexId) a
      -- ^ 'Nothing' means the beginning
    | Remove VertexId
    deriving (Show)

data RGA a
    = OpAddAfter (Maybe VertexId) a VertexId
      -- ^ - id of previous vertex, 'Nothing' means the beginning
      --   - atom
      --   - id of this vertex
    | OpRemove VertexId
    deriving (Eq, Show)

instance CausalOrd (RGA a) where
    precedes _ _ = False

emptyPayload :: RgaPayload a
emptyPayload = RgaPayload {vertices = Vector.empty, vertexIxs = Map.empty}

instance (AsEmpty a, Ord a) => CmRDT (RGA a) where
    type Intent  (RGA a) = RgaIntent  a
    type Payload (RGA a) = RgaPayload a

    initial = emptyPayload

    makeOp (AddAfter mOldId atom) payload = case mOldId of
        Nothing -> ok
        Just oldId
            | lookup oldId payload -> ok
            | otherwise            -> Nothing
      where
        RgaPayload{vertexIxs} = payload
        ok = Just $ do
            case Map.lookupMax vertexIxs of
                Just (LamportTime maxKnownTime _, _) -> advance maxKnownTime
                Nothing                              -> pure ()
            OpAddAfter mOldId atom <$> getTime

    makeOp (Remove w) payload
        | lookup w payload = Just . pure $ OpRemove w
        | otherwise        = Nothing

    apply (OpAddAfter mOldId newAtom newId) payload =
        RgaPayload{vertices = vertices', vertexIxs = vertexIxs'}
      where
        RgaPayload{vertices, vertexIxs} = payload
        n = length vertices

        (vertices', newIx)
            | null vertices = case mOldId of
                Nothing    -> (Vector.singleton (newId, newAtom), 0)
                Just oldId -> error $ show oldId <> " not delivered"
            | otherwise = (insert ix, ix)
              where
                ix = findWhereToInsert $ case mOldId of
                    Nothing    -> 0
                    Just oldId -> vertexIxs Map.! oldId + 1

        vertexIxs' = Map.insert newId newIx $ Map.map shift vertexIxs

        shift ix
            | ix >= newIx = ix + 1
            | otherwise   = ix

        -- Find an edge (l, r) within which to splice new
        findWhereToInsert ix =
            case vertices Vector.!? ix of
                Just (t', _) | newId < t' -> -- Right position, wrong order
                    findWhereToInsert $ succ ix
                _ -> ix

        insert ix
            | ix < n = left <> Vector.singleton (newId, newAtom) <> right
            | otherwise = Vector.snoc vertices (newId, newAtom)
          where
            (left, right) = Vector.splitAt ix vertices

    apply (OpRemove vid) payload@RgaPayload{vertices, vertexIxs} =
        -- pre addAfter(_, w) delivered  -- 2P-Set precondition
        payload{vertices = vertices // [(ix, (vid, empty))]}
      where
        ix = vertexIxs Map.! vid

fromList
    :: (AsEmpty a, Ord a, Clock m, MonadFail m, MonadState (RgaPayload a) m)
    => [a]
    -> m [RGA a]
fromList = go Nothing
  where
    go _      []     = pure []
    go prevId (x:xs) = do
        op@(OpAddAfter _ _ newId) <- makeAndApplyOp (AddAfter prevId x)
        (op :) <$> go (Just newId) xs

toList :: RgaPayload a -> [a]
toList RgaPayload { vertices } = map snd $ Vector.toList vertices

toVector :: RgaPayload a -> Vector a
toVector RgaPayload { vertices } = Vector.map snd vertices

fromString
    :: (Clock m, MonadFail m, MonadState (RgaPayload Char) m)
    => String
    -> m [RGA Char]
fromString = fromList

toString :: RgaPayload Char -> String
toString = toList

load :: Vector (VertexId, a) -> RgaPayload a
load vertices = RgaPayload
    { vertices
    , vertexIxs = Map.fromList
        [ (vid, ix) | ix <- [0..] | (vid, _) <- Vector.toList vertices ]
    }
