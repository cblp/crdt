{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}

module Data.Empty where

-- | A type that may be empty.
-- If your type does not have a special empty value, just wrap it into 'Maybe',
-- it is free.
--
-- Based on Control.Lens.Empty.AsEmpty.
class AsEmpty a where
    empty :: a

    isEmpty :: a -> Bool
    default isEmpty :: Eq a => a -> Bool
    isEmpty a = empty == a

    isNotEmpty :: a -> Bool
    default isNotEmpty :: Eq a => a -> Bool
    isNotEmpty a = empty /= a

instance AsEmpty (Maybe a) where
    empty = Nothing

    isEmpty = \case
        Nothing -> True
        _       -> False

    isNotEmpty = \case
        Nothing -> False
        _       -> True

instance AsEmpty Char where
    empty = '\NUL'
