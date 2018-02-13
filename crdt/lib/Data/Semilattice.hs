module Data.Semilattice
    ( Semilattice
    , merge
    ) where

import           Data.Semigroup (Max, Semigroup, (<>))
import           Data.Set (Set)

{- |
A semilattice.

It may be a join-semilattice, or meet-semilattice, it doesn't matter.

If it matters for you, use package @lattices@.

In addition to 'Semigroup', Semilattice defines this laws:

[commutativity]

    @x '<>' y == y '<>' x@

[idempotency]

    @x '<>' x == x@
-}
class Semigroup a => Semilattice a

-- | Just ('Semigroup.<>'), specialized to 'Semilattice'.
merge :: Semilattice a => a -> a -> a
merge = (<>)
infixr 6 `merge`
{-# INLINE merge #-}

-- instances for external types

instance Ord a => Semilattice (Max a)

instance Ord a => Semilattice (Set a)
