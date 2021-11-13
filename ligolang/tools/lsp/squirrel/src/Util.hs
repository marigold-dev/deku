module Util
  ( foldMapM
  , unconsFromEnd
  , safeIndex
  , toUri
  , toLocation
  , unionOrd
  , findKey
  , removeDots

  -- * Debugging utilities
  , validate
  ) where

import Data.Foldable (foldlM)
import Data.Map.Internal qualified as MI
import Language.LSP.Types qualified as J
import System.Directory (canonicalizePath)
import System.IO.Unsafe (unsafePerformIO)
import Witherable (ordNub)

import Duplo.Lattice
import Duplo.Tree

import Range

foldMapM :: (Foldable t, Monad m, Monoid b) => (a -> m b) -> t a -> m b
foldMapM f = foldlM folder mempty
  where
    folder !acc new = (acc <>) <$> f new

-- | Split into list into a pair of all but the last elements, and the last
-- element.
--
-- @unconsFromEnd [1, 2, 3] = Just ([1, 2], 3)@
unconsFromEnd :: [a] -> Maybe ([a], a)
unconsFromEnd [] = Nothing
unconsFromEnd xs = Just (init xs, last xs) -- doubt we should care about two passes

safeIndex :: (Eq t, Num t) => [a] -> t -> Maybe a
safeIndex [] _ = Nothing
safeIndex (x : _) 0 = Just x
safeIndex (_ : xs) n = safeIndex xs (n - 1)

toUri :: Range -> J.Uri
toUri = J.filePathToUri . _rFile

toLocation :: Range -> J.Location
toLocation = J.Location <$> toUri <*> toLspRange

-- | Takes the union of two lists, leaving no duplicates using the provided 'Ord'
-- instance.  O((m + n) log (m + n)) complexity.
unionOrd :: Ord a => [a] -> [a] -> [a]
unionOrd as bs = ordNub (as <> bs)

-- | Find a key in a map by testing for equality using some projection.
-- O(log n) assuming the projection function is O(1).
--
-- N.B.: It is required that the projection function preserves the order of the
-- Map, that is, for all keys a < b < c < ... in the map, it is required that
-- f a < f b < f c < ....
findKey :: (Ord k, Ord k') => (k -> k') -> k' -> MI.Map k v -> Maybe (k, v)
findKey _ _ MI.Tip = Nothing
findKey f x (MI.Bin _ k v l r) = case compare x (f k) of
  LT -> findKey f x l
  EQ -> Just (k, v)
  GT -> findKey f x r

removeDots :: FilePath -> FilePath
{-# NOINLINE removeDots #-}
removeDots = unsafePerformIO . canonicalizePath

-- | Throws an error if the tree contains any subtrees such that the ranges are
-- not smaller than its parent nodes, or returns the tree unmodified, otherwise.
--
-- The error might be useful for debugging, as it will include the offending
-- ranges.
--
-- Warning: Use only for debugging.
validate :: (Functor f, Lattice a, Show a) => Cofree f a -> Cofree f a
validate (info :< tree) = info :< fmap (go info) tree
  where
    go info' (info'' :< tree')
      | info'' `leq` info' = info'' :< fmap (go info'') tree'
      | otherwise = error $ show info'' <> " ≰ " <> show info'
