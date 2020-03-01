{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ParallelListComp #-}

module CRDT.Cv.RGA
    ( RGA (..)
    , fromList
    , toList
    , edit
    , RgaString
    , fromString
    , toString
    -- * Packed representation
    , RgaPacked
    , pack
    , unpack
    ) where

import           Data.Algorithm.Diff (PolyDiff (Both, First, Second),
                                      getGroupedDiffBy)
import           Data.Empty (AsEmpty (..))
import           Data.Function (on)
import           Data.Semilattice (Semilattice)
import           Data.Traversable (for)

import           CRDT.LamportClock (Clock, LamportTime (LamportTime), getTimes)

type VertexId = LamportTime

-- | TODO(cblp, 2018-02-06) Vector.Unboxed
newtype RGA a = RGA [(VertexId, a)]
    deriving (Eq, Show)

type RgaString = RGA Char

merge :: (Eq a, AsEmpty a) => RGA a -> RGA a -> RGA a
merge (RGA vertices1) (RGA vertices2) = RGA
    $ mergeVertexLists vertices1 vertices2
  where
    mergeVertexLists []  vs2 = vs2
    mergeVertexLists vs1 []  = vs1
    mergeVertexLists (v1@(id1, a1):vs1) (v2@(id2, a2):vs2) =
        case compare id1 id2 of
            LT -> v2 : mergeVertexLists (v1 : vs1) vs2
            GT -> v1 : mergeVertexLists vs1 (v2 : vs2)
            EQ -> (id1, mergeAtoms a1 a2) : mergeVertexLists vs1 vs2

    -- priority of deletion
    mergeAtoms a1 a2 | isEmpty a1 || isEmpty a2 = empty
                     | a1 == a2                 = a1
                     | otherwise                = empty -- error: contradiction

instance (Eq a, AsEmpty a) => Semigroup (RGA a) where
    (<>) = merge

instance (Eq a, AsEmpty a) => Semilattice (RGA a)

-- Why not?
instance (Eq a, AsEmpty a) => Monoid (RGA a) where
    mempty = RGA []
    mappend = (<>)

toList :: AsEmpty a => RGA a -> [a]
toList (RGA rga) = [ a | (_, a) <- rga, isNotEmpty a ]

toString :: RgaString -> String
toString = toList

fromList :: Clock m => [a] -> m (RGA a)
fromList = fmap RGA . fromList'

fromList' :: Clock m => [a] -> m [(VertexId, a)]
fromList' xs = do
    LamportTime time0 pid <- getTimes . fromIntegral $ length xs
    pure [ (LamportTime time pid, x) | time <- [time0..] | x <- xs ]

fromString :: Clock m => String -> m RgaString
fromString = fromList

-- | Replace content with specified,
-- applying changed found by the diff algorithm
edit :: (Eq a, AsEmpty a, Clock m) => [a] -> RGA a -> m (RGA a)
edit newList (RGA oldRga) = fmap (RGA . concat) . for diff $ \case
    First removed -> pure [ (vid, empty) | (vid, _) <- removed ]
    Both v _      -> pure v
    Second added  -> fromList' $ map snd added
  where
    newList' = [ (undefined, a) | a <- newList ]
    diff     = getGroupedDiffBy ((==) `on` snd) oldRga newList'

-- | Compact version of 'RGA'.
-- For each 'VertexId', the corresponding sequence of vetices has the same 'Pid'
-- and sequentially growing 'LocalTime', starting with the specified one.
type RgaPacked a = [(VertexId, [a])]

pack :: RGA a -> RgaPacked a
pack (RGA []                ) = []
pack (RGA ((first, atom):vs)) = go first [atom] 1 vs
  where
    -- TODO(cblp, 2018-02-08) buf :: DList
    go vid buf _ [] = [(vid, buf)]
    go vid buf dt ((wid, a):ws)
        | wid == next dt vid = go vid (buf ++ [a]) (succ dt) ws
        | otherwise          = (vid, buf) : go wid [a] 1 ws
    next dt (LamportTime t p) = LamportTime (t + dt) p

unpack :: RgaPacked a -> RGA a
unpack packed = RGA $ do
    (LamportTime time pid, atoms) <- packed
    [ (LamportTime (time + i) pid, atom) | i <- [0..] | atom <- atoms ]
