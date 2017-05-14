module CRDT.Cv
    ( CvRDT
    ) where

import Data.Semigroup (Semigroup)

{- |
State-based, or convergent (Cv) replicated data type.

Update is any function modifying @state@.

Query function is not needed. State itself is exposed.
In other words, @query = id@.

Laws:
    1. Commutativity:
        x <> y == y <> x
    2. Idempotency:
        x <> x == x

Examples:
    - PN-counter
    - G-set
    - 2P-set
    - LWW
    - LWW-set
-}
class Semigroup state => CvRDT state
