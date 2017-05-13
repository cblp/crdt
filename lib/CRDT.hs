{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Conflict-free replicated data types
module CRDT
    ( CmRDT (..)
    , CvRDT
    , query
    ) where

import Data.Kind      (Type)
import Data.Proxy     (Proxy (..))
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

{- |
Operation-based, or commutative (Cm) replicated data type.
Laws:
    Commutativity:
        update op1 . update op2 == update op2 . update op1
-}
class CmRDT op where
    type State op :: Type
    update :: op -> State op -> State op
    initial :: Proxy op -> State op

query :: forall f op . (Foldable f, CmRDT op) => f op -> State op
query = foldr update (initial (Proxy :: Proxy op))
