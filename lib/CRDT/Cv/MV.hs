module CRDT.Cv.MV
    ( MV
    , query
    ) where

import           Data.Set (Set)

import           CRDT.Cv.MV.Internal

-- query incVV() : integer[n] V'
--     let g = myID()
--     let V1 = {V | ∃x : (x, V) ∈ S}
--     let V' = [ max (V ∈ V1) (V[j]) ] j /= g
--     let V' [g] = max (V ∈ V1) (V[g]) + 1

-- update assign (set R)
--     ⊲ set of elements of type X
--     let V = incVV()
--     S := R × {V}

query :: MV a -> Set (a, Version)
query (MV a) = a

-- compare (A, B) : boolean b
--     let b = (∀(x, V) ∈ A, (x', V') ∈ B : V ≤ V')
