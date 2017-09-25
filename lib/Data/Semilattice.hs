module Data.Semilattice
    ( Semilattice
    , slappend
    ) where

import           Data.Semigroup (Semigroup, (<>))

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
slappend :: Semilattice a => a -> a -> a
slappend = (<>)
infixr 6 `slappend`
{-# INLINE slappend #-}
