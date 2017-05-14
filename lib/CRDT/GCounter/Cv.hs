module CRDT.GCounter.Cv
    ( GCounter
    , increment
    , initial
    , query
    ) where

import           Data.Monoid         ((<>))
import qualified Data.Vector         as Vector
import qualified Data.Vector.Mutable as VectorM

import CRDT.GCounter.Cv.Internal

increment :: (Enum a, Num a) => Word -> GCounter a -> GCounter a
increment replicaId (GCounter vec) = let
    i = fromIntegral replicaId
    vecResized =
        if i + 1 > length vec then
            vec <> Vector.replicate (i + 1 - length vec) 0
        else
            vec
    vecUpdated = Vector.modify (\vm -> VectorM.modify vm succ i) vecResized
    in
    GCounter vecUpdated

initial :: GCounter a
initial = GCounter Vector.empty

query :: Num a => GCounter a -> a
query (GCounter v) = sum v
