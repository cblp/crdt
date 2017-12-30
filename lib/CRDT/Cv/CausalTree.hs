{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- | Causal trees as described by Victor Grishchenko
-- in http://www.ds.ewi.tudelft.nl/~victor/polo.pdf
module CRDT.Cv.CausalTree
    (
    -- * Definition
      CausalTree
    -- * Construction
    , emptyCausalTree
    -- * Modification
    -- * Query
    -- * Utility
    , CTString
    -- * Internal
    , Atom (..)
    , Atom1
    , Atom3 (..)
    , Atom3c (..)
    , Atom3s (..)
    , Atom5 (..)
    , Atom5c (..)
    , AtomId (..)
    , AtomRef (..)
    , braid
    , diff
    , Patch1
    , Patch3c
    , Patch5c
    -- , pull
    , scour
    , spin
    , stringify
    , Text1
    , Text3
    -- , weave
    , Weave5c
    -- , Weft2
    , weftI
    , Yarn3c
    , Yarn5
    , Yarn5c
    ) where

import           Control.Monad (unless)
import           Control.Monad.ST (runST)
import           Data.Algorithm.Diff (Diff (Both, First, Second), getDiff)
import           Data.Foldable (for_)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           GHC.Stack (HasCallStack)
-- import           Data.Semigroup (Semigroup, (<>))
import           Data.STRef (modifySTRef, newSTRef, readSTRef, writeSTRef)
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Numeric.Natural (Natural)

import           CRDT.LamportClock (Pid)
-- import           Data.Semilattice (Semilattice)

data Atom c
    = Atom c
    | Aware -- ^ ⌀
    | Backspace -- ^ ⌫
    | Undelete  -- ^ ⌦
    -- Start    -- ^ ॐ
    -- End      -- ^ ۝
    deriving (Eq, Show)

startRef :: AtomRef
startRef = AtomRef{yarn = Nothing, offset = 0}

-- | Atom1 is just an atom, provided for consistency.
type Atom1 = Atom

-- | Atom2 form — atom reference — atom's yarn and offset.
-- {yarn = Nothing} is a reference to the "special" yarn.
data AtomRef = AtomRef
    { yarn   :: !(Maybe Pid)
    , offset :: !Natural
    }
    deriving (Eq, Show)

-- | Atom2 form — atom's identifier — atom's yarn and offset.
data AtomId = AtomId
    { yarn   :: !Pid
    , offset :: !Natural
    }
    deriving (Eq, Show)

atomRef :: AtomId -> AtomRef
atomRef AtomId{..} = AtomRef{yarn = Just yarn, ..}

-- | Atom3 form — character with its identifier.
data Atom3 c = Atom3
    { atom  ::                !(Atom c)
    , self  :: {-# UNPACK #-} !AtomId
    }
    deriving (Eq, Show)

-- | A character and its cause.
data Atom3c c = Atom3c
    { atom   ::                !(Atom c)
    , parent :: {-# UNPACK #-} !AtomRef
    }
    deriving (Eq, Show)

data Atom3s c = Atom3s
    { atom  ::                !c
    , self  :: {-# UNPACK #-} !AtomId
    }
    deriving (Eq, Show)

data Atom5 c = Atom5
    { atom   ::                !(Atom c)
    , self   :: {-# UNPACK #-} !AtomId
    , parent :: {-# UNPACK #-} !AtomRef
    }
    deriving Eq

data Atom5c c = Atom5c
    { atom   ::                !(Atom c)
    , parent :: {-# UNPACK #-} !AtomRef
    , self   :: {-# UNPACK #-} !AtomId
    }
    deriving (Eq, Show)

-- TODO(cblp, 2018-01-02) try unboxed vector
type Yarn3c c = Vector (Atom3c c)

type Yarn5 c = Vector (Atom5 c)

type Yarn5c c = Vector (Atom5c c)

-- | Weft which is effectively a revision’s identifier.
-- 'Weft2' is the last character id for each yarn, except special.
type Weft2 = Map Pid Natural

-- | The 'WeftI' format is more condensed than 'Weft2'.
-- It only includes the last symbol ids,
-- sorted according to the alphanumeric ordering of their yarn URLs.
type WeftI = [Natural]

type Text1 = []

-- TODO(cblp, 2018-01-02) exclude special atoms
type Text3 c = [Atom3s c]

-- Patch for 'Atom1' level
type Patch1 c = [Diff c]

-- Patch for 'Atom3c' level
type Patch3c c = [Atom3c c]

-- Patch for 'Atom5c' level
type Patch5c c = [Atom5c c]

type Weave5c c = Vector (Atom5c c)

-- TODO(cblp, 2018-01-02)
-- instance Ord c => Semigroup (Weave5c c) where
--     Weave5c ct1 <> Weave5c ct2 =
--         Weave5c $ Map.unionWith (<>) ct1 ct2

-- TODO(cblp, 2018-01-02)
-- instance Ord c => Semilattice (Weave5c c)

-- TODO(cblp, 2018-01-02) -- | append t s ~ t ++ s
-- append :: Ord c => Weave5c c -> [c] -> Process (Weave5c c)
-- append ct chars = pidReader $ append' ct chars
--
-- append' :: Ord c => Weave5c c -> [c] -> Pid -> Weave5c c
-- append' ct []    _   = ct
-- append' ct chars pid = _

-- TODO(cblp, 2018-01-02)
-- | prepend s t ~ s ++ t
-- prepend :: Ord c => [c] -> Weave5c c -> Process (Weave5c c)
-- prepend chars ct = pidReader $ prepend' chars ct
--
-- prepend' :: Ord c => [c] -> Weave5c c -> Pid -> Weave5c c
-- prepend' []    ct             _   = ct
-- prepend' chars weave5c pid =
--     Weave5c{yarns = Map.adjust (<> newAtoms) pid yarns}
--   where
--     yarnLength = maybe 0 (fromIntegral . length) $ Map.lookup pid yarns
--     parentYarnIds = Nothing : repeat (Just pid)
--     parentOffsets = 0       : [yarnLength ..]
--     parents = zipWith AtomId parentYarnIds parentOffsets
--     newAtoms = foldMap Vector.singleton $ zipWith Atom3c chars parents

newtype CausalTree c = CausalTree (Weave5c c)
type CTString = CausalTree Char

emptyCausalTree :: CausalTree c
emptyCausalTree = CausalTree Vector.empty

-- TODO(cblp, 2018-01-02)
-- yarn3c :: Yarn5 -> Yarn3c

-- TODO(cblp, 2018-01-02)
-- tailCut3c :: Yarn3c -> Patch3c

-- TODO(cblp, 2018-01-02)
-- tailCut5c :: Yarn5 -> Patch5c

-- TODO(cblp, 2018-01-02)
-- filtre :: Weft2 -> Filtre

braid :: Pid -> Natural -> Patch3c c -> Patch5c c
braid yarn currentYarnLength = zipWith f [currentYarnLength ..]
  where
    f offset Atom3c{..} = Atom5c{self = AtomId{yarn, offset}, ..}

-- TODO(cblp, 2018-01-02)
-- patch :: Patch5c -> Weave5c

-- TODO(cblp, 2018-01-03) Use Either for error report, remove Show dependency
spin
    :: ( Eq c
       , HasCallStack, Show c -- only for error
       )
    => Text3 c -> Patch1 c -> Patch3c c
spin = go startRef
  where
    go prev t p = case p of
        [] -> case t of -- end
            [] -> []
            _ -> error "inconsistent patch"
        Both c _ : p' -> case t of -- no change
            Atom3s{atom, self} : t'
                | c == atom -> go (atomRef self) t' p'
                | otherwise -> error "inconsistent patch"
            [] -> error "inconsistent patch"
        First c : p' -> case t of -- delete
            Atom3s{atom, self} : t'
                | c == atom -> let
                    parent = atomRef self
                    a = Atom3c{atom = Backspace, parent}
                    in
                    a : go parent t' p'
                | otherwise -> error "inconsistent patch"
            [] -> error "inconsistent patch"
        Second c : p' -> -- insert
            Atom3c{atom = Atom c, parent = prev} : go prev t p'

-- TODO(cblp, 2018-01-02)
-- ravel :: Weave5c -> Yarn5

-- TODO(cblp, 2018-01-02)
-- pull :: Weave5c c -> Weft2
-- pull = undefined

-- TODO(cblp, 2018-01-02)
-- dye :: Weave5c -> Text3h

-- | 4.1 Scouring (weave to text)
-- RE: s/(...(..))(⌫\2..)+|[⌫⌀].{4}|(.)..(..)/$4$5/g
-- TODO(cblp, 2018-01-02) This regex is simplified for the ease of
-- understanding; it ignores undeletions (see Sec. 4.6).
scour :: Weave5c c -> Text3 c
scour weave = runST $ do
    result <- newSTRef Vector.empty -- TODO(cblp, 2018-01-02) try mutable? DList?
    buf <- newSTRef Nothing
    let appendResult atom = modifySTRef result (`Vector.snoc` atom)
    for_ weave $ \Atom5c{atom, parent, self} ->
        readSTRef buf >>= \case
            Nothing -> case atom of
                Atom c    ->
                    writeSTRef buf $ Just (Atom3s{atom = c, self}, False)
                Aware     -> pure ()
                Backspace -> pure ()
                _         -> error
                    "This regex is simplified for the ease of understanding;\
                    \ it ignores undeletions (see Sec. 4.6)."
            Just (bufAtom@Atom3s{self = previous}, inBackspaces) ->
                case atom of
                    Atom c ->
                        flushAndAdd $
                        Just (Atom3s{atom = c, self}, inBackspaces)
                    Backspace | parent == atomRef previous ->
                        writeSTRef buf $ Just (bufAtom, True)
                    _ -> flushAndAdd Nothing
              where
                flushAndAdd bufNew = do
                    unless inBackspaces $ appendResult bufAtom
                    writeSTRef buf bufNew
    readSTRef buf >>= \case
        Just (atom, _) -> appendResult atom
        _              -> pure ()
    Vector.toList <$> readSTRef result

-- TODO(cblp, 2018-01-02)
-- weftCut :: Weave5c -> Weave5c

diff :: Eq c => Text1 c -> [c] -> Patch1 c
diff = getDiff

stringify :: Text3 c -> Text1 c
stringify = map $ \Atom3s{atom} -> atom

-- TODO(cblp, 2018-01-02)
-- text1 :: Text3h -> Text1

weftI :: Weft2 -> WeftI
weftI = Map.elems
