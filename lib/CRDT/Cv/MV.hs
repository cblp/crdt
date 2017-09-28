module CRDT.Cv.MV
    ( MV
    , query
    ) where

import           Algebra.Lattice.Ordered (Ordered)
import           Algebra.PartialOrd (PartialOrd (leq))
import           Data.Foldable (toList)
import           Data.IntMap (IntMap)
import           Data.Semigroup (Semigroup ((<>)))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Numeric.Natural (Natural)

type Version = IntMap (Ordered Natural)

-- | Multi-value register
data MV a = MV (Set (a, Version))

-- payload set S
--     ⊲ set of (x, V) pairs; x ∈ X; V its version vector
--     initial {(⊥, [0, . . . , 0])}

-- query incVV () : integer[n] V′
--     let g = myID()
--     let V = {V |∃x : (x, V) ∈ S}
--     let V′ = [max V ∈V (V [j])] j /= g
--     let V′ [g] = max V ∈V (V [g]) + 1

-- update assign (set R)
--     ⊲ set of elements of type X
--     let V = incVV ()
--     S := R × {V }

query :: MV a -> Set (a, Version)
query (MV a) = a

-- compare (A, B) : boolean b
--     let b = (∀(x, V) ∈ A, (x′, V′) ∈ B : V ≤ V′)

instance Ord a => Semigroup (MV a) where
    MV a <> MV b =
        let a' = Set.filter (not . dominatedBy b) a
            b' = Set.filter (not . dominatedBy a) b
        in  MV $ Set.union a' b'
      where
        dominatedBy c (_, v) = or [leq v w | (_, w) <- toList c]
