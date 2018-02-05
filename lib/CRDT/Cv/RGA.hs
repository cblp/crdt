{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module CRDT.Cv.RGA
    ( RGA (..)
    , RgaString
    , edit
    , fromList
    , fromString
    , toList
    , toString
    ) where

import           Data.Algorithm.Diff (Diff (Both, First, Second), getDiffBy)
import           Data.Function (on)
import           Data.Semigroup (Semigroup, (<>))
import           Data.Semilattice (Semilattice)
import           Data.Traversable (for)

import           CRDT.LamportClock (Clock, LamportTime, getTime)

type VertexId = LamportTime

-- | TODO(cblp, 2018-02-06) Vector.Unboxed
newtype RGA a = RGA [(VertexId, Maybe a)]
    deriving (Eq, Show)

type RgaString = RGA Char

merge :: Eq a => RGA a -> RGA a -> RGA a
merge (RGA vertices1) (RGA vertices2) =
    RGA $ mergeVertexLists vertices1 vertices2
  where
    mergeVertexLists []                   vs2                  = vs2
    mergeVertexLists vs1                  []                   = vs1
    mergeVertexLists (v1@(id1, a1) : vs1) (v2@(id2, a2) : vs2) =
        case compare id1 id2 of
            LT -> v2                      : mergeVertexLists (v1:vs1) vs2
            GT -> v1                      : mergeVertexLists vs1      (v2:vs2)
            EQ -> (id1, mergeAtoms a1 a2) : mergeVertexLists vs1      vs2

    mergeAtoms (Just a1) (Just a2)
        | a1 == a2 = Just a1
    mergeAtoms _ _ = Nothing

instance Eq a => Semigroup (RGA a) where
    (<>) = merge

instance Eq a => Semilattice (RGA a)

-- Why not?
instance Eq a => Monoid (RGA a) where
    mempty = RGA []
    mappend = (<>)

toList :: RGA a -> [a]
toList (RGA rga) = [a | (_, Just a) <- rga]

toString :: RgaString -> String
toString = toList

fromList :: Clock m => [a] -> m (RGA a)
fromList = fmap RGA . traverse makeVertex
  where
    makeVertex a = do
        t <- getTime
        pure (t, Just a)

fromString :: Clock m => String -> m RgaString
fromString = fromList

-- | Replace content with specified,
-- applying changed found by the diff algorithm
edit :: (Eq a, Clock m) => [a] -> RGA a -> m (RGA a)
edit newList (RGA oldRga) =
    fmap RGA $ for diff $ \case
        First  (vid, _)        -> pure (vid, Nothing)
        Both   v        _      -> pure v
        Second          (_, a) -> (, a) <$> getTime
  where
    newList' = [(undefined, Just a) | a <- newList]
    diff = getDiffBy ((==) `on` snd) oldRga newList' -- TODO(cblp, 2018-02-07) getGroupedDiffBy
