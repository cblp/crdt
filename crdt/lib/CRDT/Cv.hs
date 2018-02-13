{-# LANGUAGE ConstraintKinds #-}

module CRDT.Cv
    ( CvRDT
    ) where

import           Data.Semilattice (Semilattice)

{- |
State-based, or convergent (Cv) replicated data type.

Update is any function modifying @state@.

Query function is not needed. State itself is exposed.
In other words, @query = 'id'@.
Some types may offer more convenient query functions.

Actually, a CvRDT is nothing more a 'Semilattice'.
-}
type CvRDT = Semilattice
