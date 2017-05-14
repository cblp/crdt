module CRDT.Cv
    ( CvRDT
    ) where

import Data.Semigroup (Semigroup (..))

{- |
State-based, or convergent (Cv) replicated data type.

Update is any function modifying @state@.

Query function is not needed. State itself is exposed.
In other words, @query = 'id'@.

Laws:

[commutativity]

    @x '<>' y == y '<>' x@

[idempotency]

    @x '<>' x == x@
-}
class Semigroup state => CvRDT state
