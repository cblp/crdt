{-@ LIQUID "--automatic-instances=liquidinstances" @-}
{-@ LIQUID "--exact-data-cons" @-}
{-@ LIQUID "--higherorder" @-}
{-@ LIQUID "--higherorderqs" @-}

module Max where

import           Data.Set as Set
import           Language.Haskell.Liquid.ProofCombinators

data L a = Emp | a ::: L a
{-@ data L [llen] @-}

data Max a = Max a

{-@ measure llen @-}
{-@ llen :: L a -> Nat @-}
llen :: L a -> Int
llen Emp        = 0
llen (_ ::: xs) = 1 + llen xs

{-@ axiomatize appendL @-}
appendL :: L a -> L a -> L a
appendL Emp        ys = ys
appendL (x ::: xs) ys = x ::: appendL xs ys

{-@ axiomatize appendM @-}
appendM :: Ord a => Max a -> Max a -> Max a
appendM (Max x) (Max y) = Max (if x >= y then x else y)

{-@ axiomatize appendS @-}
appendS :: Ord a => Set a -> Set a -> Set a
appendS x y = Set.union x y

{-@ appendAssocL
    :: xs : L a
    -> ys : L a
    -> zs : L a
    -> {appendL (appendL xs ys) zs == appendL xs (appendL ys zs)} @-}
appendAssocL :: L a -> L a -> L a -> Proof
appendAssocL Emp ys zs = trivial
appendAssocL (x ::: xs) ys zs = appendAssocL xs ys zs

{-@ appendAssocM
    :: x : Max a
    -> y : Max a
    -> z : Max a
    -> {appendM (appendM x y) z == appendM x (appendM y z)} @-}
appendAssocM :: Max a -> Max a -> Max a -> Proof
appendAssocM (Max x) (Max y) (Max z) = trivial

{-@ appendAssocS
    :: x : Set a
    -> y : Set a
    -> z : Set a
    -> {appendS (appendS x y) z == appendS x (appendS y z)} @-}
appendAssocS :: Set a -> Set a -> Set a -> Proof
appendAssocS _ _ _ = trivial
