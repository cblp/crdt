{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

-- | Replicated Growable Array (RGA)
module CRDT.Cm.RGA
    ( RGA (..)
    , RgaIntent (..)
    , RgaPayload (..)
    , fromString
    , toString
    ) where

import           Prelude hiding (lookup)

import           Control.Monad.Fail (MonadFail)
import           Control.Monad.State.Strict (MonadState)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes)

import           CRDT.Cm (CausalOrd, CmRDT, Intent, Payload, apply, initial,
                          makeAndApplyOp, makeOp, precedes)
import           CRDT.LamportClock (Clock, LamportTime (LamportTime), advance,
                                    getTime)

-- | Using 'LamportTime' as an identifier for vertices
type VertexId = LamportTime

data RgaPayload a = RgaPayload
    { vertices :: Map VertexId (Maybe a)
    , first    :: Maybe VertexId
      -- ^ the Vertex just after the beginning
    , edges    :: Map VertexId VertexId
      -- ^ the link to the next vertex
    }
    deriving (Eq, Show)

-- | Is added and is not removed.
lookup :: VertexId -> RgaPayload a -> Bool
lookup v RgaPayload{vertices} =
    case Map.lookup v vertices of
        Just Just{} -> True
        _ -> False

-- before :: Vertex -> Vertex -> Bool
-- before u v = b
--     pre lookup(u) ∧ lookup(v)
--     ∃w_1, ..., w_m ∈ verticesAdded:
--         w_1 = u
--         ∧ w_m = v
--         ∧ ∀j: (w_j, w_{j+1}) ∈ edges

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

instance Ord a => CmRDT (RGA a) where
    type Intent  (RGA a) = RgaIntent  a
    type Payload (RGA a) = RgaPayload a

    initial = RgaPayload
        { vertices = Map.empty
        , first    = Nothing
        , edges    = Map.empty
        }

    makeOp (AddAfter mOldId atom) payload = case mOldId of
        Nothing -> ok
        Just oldId
            | lookup oldId payload -> ok
            | otherwise            -> Nothing
      where
        RgaPayload{vertices} = payload
        ok = Just $ do
            case Map.lookupMax vertices of
                Just (LamportTime maxKnownTime _, _) ->
                    advance maxKnownTime
                Nothing -> pure ()
            newId <- getTime
            pure $ OpAddAfter mOldId atom newId

    makeOp (Remove w) payload
        | lookup w payload = Just . pure $ OpRemove w
        | otherwise        = Nothing

    apply (OpAddAfter oldId newAtom newId) payload = RgaPayload
        { vertices = Map.insert newId (Just newAtom) vertices
        , first    = first'
        , edges    = edges'
        }
      where
        (first', edges') = insert oldId (successor oldId)
        RgaPayload{vertices, first, edges} = payload

        insert
            :: Maybe VertexId
            -> Maybe VertexId
            -> (Maybe VertexId, Map VertexId VertexId) -- (first, edges)
        insert l r = case r of -- Find an edge (l, r) within which to splice new
            Just t' | newId < t' -> -- Right position, wrong order
                insert r (successor r)
            _ -> insertBetween l r

        insertBetween
            :: Maybe VertexId
            -> Maybe VertexId
            -> (Maybe VertexId, Map VertexId VertexId) -- (first, edges)
        insertBetween (Just l) (Just r) =
            (first, Map.insert l newId . Map.insert newId r $ edges)
        insertBetween (Just l) Nothing = (first, Map.insert l newId edges)
        insertBetween Nothing (Just r) = (Just newId, Map.insert newId r edges)
        insertBetween Nothing Nothing = (Just newId, edges)

        successor = \case
            Nothing -> first
            Just vid
                | lookup vid payload -> Map.lookup vid edges
                | otherwise          -> Nothing

    apply (OpRemove vid) payload@RgaPayload{vertices} =
        -- pre addAfter(_, w) delivered  -- 2P-Set precondition
        payload{vertices = Map.insert vid Nothing vertices}

fromList
    :: (Ord a, Clock m, MonadFail m, MonadState (RgaPayload a) m)
    => [a] -> m [RGA a]
fromList = go Nothing
  where
    go _      []     = pure []
    go prevId (x:xs) = do
        op@(OpAddAfter _ _ newId) <- makeAndApplyOp (AddAfter prevId x)
        (op :) <$> go (Just newId) xs

toList :: RgaPayload a -> [a]
toList RgaPayload{first, vertices, edges} = case first of
    Nothing -> []
    Just firstId -> catMaybes $ catMaybes
        [Map.lookup vid vertices | vid <- firstId : go firstId]
  where
    go vid = case Map.lookup vid edges of
        Nothing   -> []
        Just next -> next : go next

fromString
    :: (Clock m, MonadFail m, MonadState (RgaPayload Char) m)
    => String -> m [RGA Char]
fromString = fromList

toString :: RgaPayload Char -> String
toString = toList
