module CRDT.GCounter
    ( GCounter
    , increment
    , initial
    , query
    ) where

import           Data.Monoid         ((<>))
import qualified Data.Vector         as Vector
import qualified Data.Vector.Mutable as VectorM

import CRDT.GCounter.Internal

increment :: (Enum a, Num a) => Word -> GCounter a -> GCounter a
increment i (GCounter v) = GCounter $ Vector.modify incrementM $ if i + 1 > n then v <> Vector.replicate (fromIntegral $ i + 1 - n) 0 else v
  where
    incrementM vm = VectorM.modify vm succ $ fromIntegral i
    n = fromIntegral $ length v

initial :: GCounter a
initial = GCounter Vector.empty

query :: Num a => GCounter a -> a
query (GCounter v) = sum v
