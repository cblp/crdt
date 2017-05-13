{-# LANGUAGE TypeFamilies #-}

-- | Conflict-free replicated data types
module CRDT
    ( CmRDT (..)
    , CvRDT
    , query
    ) where

import           Data.Semigroup (Semigroup)
import Data.Kind (Type)

{- |
State-based, or convergent (Cv) replicated data type.
Laws:
    1. Commutativity:
        x <> y == y <> x
    2. Idempotency:
        x <> x == x

Examples:
    - G-counter
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

query :: (Foldable f, CmRDT op) => f op -> State op -> State op
query ops initial = foldr update initial ops
