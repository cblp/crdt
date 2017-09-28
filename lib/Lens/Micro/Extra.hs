{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE TypeFamilies #-}

module Lens.Micro.Extra ((<<+=)) where

import           Control.Monad.State (MonadState)
import           Data.EnumMap.Strict (EnumMap, enumMapToIntMap, intMapToEnumMap)
import           Lens.Micro (LensLike', at, ix)
import           Lens.Micro.GHC ()
import           Lens.Micro.Internal (At, Index, IxValue, Ixed)
import           Lens.Micro.Mtl ((<<%=))

(<<+=) :: (Num a, MonadState s m) => LensLike' ((,) a) s a -> a -> m a
alens <<+= n = alens <<%= (+ n)
infix 4 <<+=
{-# INLINE (<<+=) #-}

type instance Index (EnumMap k v) = k

type instance IxValue (EnumMap k v) = v

instance Enum k => Ixed (EnumMap k v) where
    ix k f m = intMapToEnumMap <$> ix (fromEnum k) f (enumMapToIntMap m)
    {-# INLINE ix #-}

instance Enum k => At (EnumMap k v) where
    at k f m = intMapToEnumMap <$> at (fromEnum k) f (enumMapToIntMap m)
    {-# INLINE at #-}
