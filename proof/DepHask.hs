{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import           Data.Semigroup (Max (..))
import           Data.Singletons (Demote, SingKind, fromSing, withSomeSing)
import           Data.Singletons.Prelude ((:++), SOrd,
                                          Sing (SCons, SNil, STuple0), sMax,
                                          (%:++))
import qualified Data.Singletons.Prelude as Singletons
import           Data.Singletons.TH (genSingletons, singletons)
import           Data.Singletons.TypeLits (Sing (SSym))
import           Data.Type.Equality ((:~:) (Refl))
import           GHC.TypeLits (AppendSymbol, Symbol)

class Semigroup a where
    type (x :: a) <> (y :: a) :: a

    (%<>) :: Sing (x :: a) -> Sing (y :: a) -> Sing (x <> y)

    associativity
        :: Sing (x :: a)
        -> Sing (y :: a)
        -> Sing (z :: a)
        -> ((x <> y) <> z) :~: (x <> (y <> z))

(<>)
    :: (SingKind m, Semigroup m)
    => Demote m
    -> Demote m
    -> Demote m
x <> y =
    withSomeSing x $ \sX ->
        withSomeSing y $ \sY ->
            fromSing (sX %<> sY)

--------------------------------------------------------------------------------

instance Semigroup () where
    type '() <> '() = '()
    STuple0 %<> STuple0 = STuple0
    associativity STuple0 STuple0 STuple0 = Refl

--------------------------------------------------------------------------------

singletons [d|
    data List a = Emp | Con a (List a)
        deriving (Show)

    infixr 5 `Con`

    appendList :: List a -> List a -> List a
    appendList Emp        ys = ys
    appendList (Con x xs) ys = Con x (appendList xs ys)
    |]

instance Semigroup (List a) where
    type xs <> ys = AppendList xs ys
    (%<>) = sAppendList

    associativity xs ys zs = case xs of
        SEmp        -> Refl
        SCon _ xs'  ->
            case associativity xs' ys zs of
                Refl -> Refl

--------------------------------------------------------------------------------

instance Semigroup [a] where
    type xs <> ys = xs :++ ys
    (%<>) = (%:++)

    associativity xs ys zs = case xs of
        SNil -> Refl
        SCons _ xs' ->
            case associativity xs' ys zs of
                Refl -> Refl

--------------------------------------------------------------------------------

instance Semigroup Symbol where
    type x <> y = AppendSymbol x y
    SSym %<> SSym = undefined
    associativity SSym SSym SSym = undefined

--------------------------------------------------------------------------------

genSingletons [''Max]

instance SOrd a => Semigroup (Max a) where
    type 'Max x <> 'Max y = 'Max (Singletons.Max x y)
    SMax x %<> SMax y = SMax (sMax x y)
    associativity = undefined
        --     (SMax x :: Sing ('Max x))
        --     (SMax y :: Sing ('Max y))
        --     (SMax z :: Sing ('Max z)) =
        -- if x >= y then Refl else Refl

--------------------------------------------------------------------------------

main :: IO ()
main = do
    print (() <> ())
    print ((1 `Con` 2 `Con` Emp) <> (3 `Con` Emp) :: List Integer)
    print ([1, 2] <> [3] :: [Integer])
    -- print ("hello" <> "world" :: Text)
    print (Max 1 <> Max 3 :: Max Integer)
