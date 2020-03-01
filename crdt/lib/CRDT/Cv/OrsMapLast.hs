{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CRDT.Cv.OrsMapLast (OrsMapLast (OrsMapLast), get, put, del) where

import           Data.Foldable (asum, for_)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Semilattice (Semilattice)

import           CRDT.LamportClock (Clock, LamportTime (LamportTime), advance,
                                    getTime)

type Tag = LamportTime

-- | "ORS-map-last" is OR-set-based map which elements are merge with the "last"
-- (LWW) strategy.
--
-- Monoid:
-- Empty map is the neutral element under the map union operation.
newtype OrsMapLast k v =
    OrsMapLast
        (Map k  -- group by key
            (Map Tag  -- unique by tag
                (Maybe  -- keep tags of deleted values
                    v)))
    deriving newtype (Eq, Monoid)
    deriving stock (Show)

instance Ord k => Semigroup (OrsMapLast k v) where
    -- Associativity:
    -- 1. Lemma: (Map.unionWith f) is associative if f is associative.
    -- 2. delWins is associative:
    --    1. delWins Nothing (delWins b c) = Nothing
    --       ^^^^^^^ ^^^^^^^
    --       delWins (delWins Nothing b) c = delWins Nothing c = Nothing
    --                ^^^^^^^ ^^^^^^^        ^^^^^^^ ^^^^^^^
    --    2. delWins (Just a) (delWins b c) = delWins b c
    --       ^^^^^^^  ^^^^
    --       delWins (delWins (Just a) b) c = delWins b c
    --                ^^^^^^^  ^^^^
    OrsMapLast m1 <> OrsMapLast m2 =
        OrsMapLast $ Map.unionWith (Map.unionWith delWins) m1 m2
        where
            -- for the same tag, delete wins
            delWins Nothing  _ = Nothing
            delWins (Just _) v = v

-- Idempotence:
-- 1. Lemma: (Map.unionWith f) is idempotent if f is idempotent.
-- 2. delWins is idempotent:
--    1. delWins Nothing Nothing = Nothing
--       ^^^^^^^ ^^^^^^^
--    2. delWins (Just v) (Just v) = Just v
--       ^^^^^^^  ^^^^
--
-- Commutativity:
-- 1. Lemma: (Map.unionWith f) is commutative if f is commutative.
-- 2. delWins is commutative if tags are unique, i.e. a tag uniquely identifies
--    added value in the system:
--    1. delWins (Just v) (Just w) where v /= w is impossible, because one tag
--       cannot identify different values.
--    2. delWins Nothing (Just v) = Nothing
--       ^^^^^^^ ^^^^^^^
--       delWins (Just v) Nothing = Nothing
--       ^^^^^^^  ^^^^
instance Ord k => Semilattice (OrsMapLast k v)

get :: Ord k => k -> OrsMapLast k v -> Maybe v
get k (OrsMapLast m) = Map.lookup k m >>= lastJustElem where

    lastJustElem :: Map a (Maybe b) -> Maybe b
    lastJustElem = asum . elemsDesc

    elemsDesc :: Map a b -> [b]
    elemsDesc = map snd . Map.toDescList

put ::
    forall k v m.
    (Ord k, Clock m) => k -> v -> OrsMapLast k v -> m (OrsMapLast k v)
put k v (OrsMapLast m) = OrsMapLast <$> Map.alterF updateTags k m where

    updateTags ::
        Maybe (Map LamportTime (Maybe v)) ->
        m (Maybe (Map LamportTime (Maybe v)))
    updateTags = fmap Just . updateTags' . fromMaybe mempty

    updateTags' :: Map LamportTime (Maybe v) -> m (Map LamportTime (Maybe v))
    updateTags' tagsValues = do
        for_ (Map.lookupMax tagsValues) $ \(LamportTime t _pid, _value) ->
            advance t
        t <- getTime
        pure $ Map.insert t (Just v) tagsValues

del :: Ord k => k -> OrsMapLast k v -> OrsMapLast k v
del k (OrsMapLast m) = OrsMapLast $ Map.alter (fmap updateTags) k m where

    updateTags :: Map Tag (Maybe v) -> Map Tag (Maybe v)
    updateTags = replaceAllWith Nothing

    replaceAllWith :: b -> Map a b -> Map a b
    replaceAllWith = (<$)
