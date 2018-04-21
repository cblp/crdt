{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}

module Data.MaybeLike where

class MaybeLike a where
    nothing :: a

    isJust :: a -> Bool
    default isJust :: Eq a => a -> Bool
    isJust a = nothing /= a

instance MaybeLike (Maybe a) where
    nothing = Nothing

    isJust = \case
        Nothing -> False
        _       -> True

instance MaybeLike Char where
    nothing = '\NUL'
