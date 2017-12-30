{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CausalTree where

import           Data.Algorithm.Diff (Diff (Both, First, Second))
import           GHC.Exts (fromList)
import           Test.Tasty.HUnit (testCase, (@?=))

import           CRDT.Cv.CausalTree
import           CRDT.LamportClock (Pid (Pid))

-- import           Laws (cvrdtLaws)

{-# ANN module "HLint: ignore Use camelCase" #-}

test_example_from_paper =
    [ testCase "scour Test"     $ scour ex1testWeave5c        @?= ex1test3
    , testCase "scour Text"     $ scour ex2textWeave5c        @?= ex2text3
    , testCase "stringify Test" $ stringify ex1test3          @?= ex1test1
    , testCase "stringify Text" $ stringify ex2text3          @?= ex2text1
    , testCase "diff"           $ diff ex1test1 "Text"        @?= ex1testPatch1
    , testCase "spin"           $ spin ex1test3 ex1testPatch1 @?= ex1testPatch3c
    , testCase "braid"          $ braid p2 0 ex1testPatch3c   @?= ex1testPatch5c
    ]
  where
    ex1testWeave5c :: Weave5c Char
    ex1testWeave5c = fromList
        [a5c' 'T' p1 0, a5c 'e' p1 0 p1 1, a5c 's' p1 1 p1 2, a5c 't' p1 2 p1 3]

    ex1test3 :: Text3 Char
    ex1test3 = [a3 'T' p1 0, a3 'e' p1 1, a3 's' p1 2, a3 't' p1 3]

    ex1test1 :: Text1 Char
    ex1test1 = "Test"

    ex1testPatch1 :: Patch1 Char
    ex1testPatch1 =
        [ Both  'T' 'T'
        , Both  'e' 'e'
        , First 's'
        , Second    'x'
        , Both  't' 't'
        ]

    ex1testPatch3c :: Patch3c Char
    ex1testPatch3c =
        [ a3cBs   p1 2
        , a3c 'x' p1 2
        ]

    ex1testPatch5c :: Patch5c Char
    ex1testPatch5c =
        [ a5cBs   p1 2 p2 0
        , a5c 'x' p1 2 p2 1
        ]

    ex2textWeave5c :: Weave5c Char
    ex2textWeave5c = fromList
        [ a5c' 'T'      p1 0
        , a5c  'e' p1 0 p1 1
        , a5c  'x' p1 1 p2 0
        , a5c  's' p1 1 p1 2
        , a5cBs    p1 2 p2 1
        , a5c  't' p1 2 p1 3
        ]

    ex2text3 :: Text3 Char
    ex2text3 = [a3 'T' p1 0, a3 'e' p1 1, a3 'x' p2 0, a3 't' p1 3]

    ex2text1 :: Text1 Char
    ex2text1 = "Text"

    a3   c     y i = Atom3s       c                        (AtomId y i)
    a3cBs  x p     = Atom3c Backspace (AtomRef (Just x) p)
    a3c  c x p     = Atom3c (Atom c)  (AtomRef (Just x) p)
    -- a3c' c         = Atom3c (Atom c)  (AtomRef Nothing  0)
    a5c  c x p y i = Atom5c (Atom c)  (AtomRef (Just x) p) (AtomId y i)
    a5c' c     y i = Atom5c (Atom c)  (AtomRef Nothing  0) (AtomId y i)
    a5cBs  x p y i = Atom5c Backspace (AtomRef (Just x) p) (AtomId y i)

    p1 = Pid 1
    p2 = Pid 2

-- TODO(cblp, 2018-01-03)
-- test_Cv = cvrdtLaws @CTString

-- TODO(cblp, 2018-01-03)
-- prop_String_roundtrip str pid = runLamportClock $ do
--     ct :: CTString <- runProcess pid $ fromList str
--     pure $ toList ct === str

-- TODO(cblp, 2018-01-03)
-- prop_append
--         (Field str1 :: Field "str1" String)
--         (Field str2 :: Field "str2" String)
--         (Field pid1 :: Field "pid1" Pid)
--         (Field pid3 :: Field "pid3" Pid) =
--     runLamportClock $ do
--         ct1 <- runProcess pid1 $ fromList str1
--         ct3 <- runProcess pid3 $ ct1 `append` str2
--         pure $
--             counterexample ("ct1: " ++ show ct1) $
--             counterexample ("ct3: " ++ show ct3) $
--             toList ct3 === str1 ++ str2

-- TODO(cblp, 2018-01-03)
-- prop_prepend
--         (Field str1 :: Field "str1" String)
--         (Field str2 :: Field "str2" String)
--         (Field pid2 :: Field "pid2" Pid)
--         (Field pid3 :: Field "pid3" Pid) =
--     runLamportClock $ do
--         ct2 <- runProcess pid2 $ fromList str2
--         ct3 <- runProcess pid3 $ str1 `prepend` ct2
--         pure $
--             counterexample ("ct2: " ++ show ct2) $
--             counterexample ("ct3: " ++ show ct3) $
--             toList ct3 === str1 ++ str2
