module CRDT.GCounter.Cv
    ( GCounter
    , initial
    , query
    -- * Operation
    , increment
    ) where

import           Data.Monoid         ((<>))
import qualified Data.Vector         as Vector
import qualified Data.Vector.Mutable as VectorM

import CRDT.GCounter.Cv.Internal

-- | Increment counter
increment
    :: Num a
    => Word -- ^ replica id
    -> GCounter a
    -> GCounter a
increment replicaId (GCounter vec) = let
    i = fromIntegral replicaId
    vecResized =
        if i + 1 > length vec then
            vec <> Vector.replicate (i + 1 - length vec) 0
        else
            vec
    vecUpdated = Vector.modify (\vm -> VectorM.modify vm (+1) i) vecResized
    in
    GCounter vecUpdated

-- | Initial state
initial :: GCounter a
initial = GCounter Vector.empty

-- | Get value from the state
query :: Num a => GCounter a -> a
query (GCounter v) = sum v
