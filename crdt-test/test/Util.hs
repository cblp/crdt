{-# LANGUAGE PatternSynonyms #-}

module Util where

import           Test.QuickCheck (Property, Testable, counterexample, property)

expectRight :: Testable a => Either String a -> Property
expectRight e = expectRightK e id

expectRightK :: Testable b => Either String a -> (a -> b) -> Property
expectRightK e f = case e of
    Left l  -> counterexample l $ property False
    Right a -> property $ f a

pattern (:-) :: a -> b -> (a, b)
pattern a :- b = (a, b)
infix 0 :-

ok :: Property
ok = property True

fail :: String -> Property
fail s = counterexample s $ property False
