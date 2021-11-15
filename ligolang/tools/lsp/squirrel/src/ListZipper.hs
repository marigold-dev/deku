module ListZipper
  ( ListZipper (..)
  , withListZipper
  , atLocus
  , find
  , here
  , next
  ) where

import Data.Maybe (listToMaybe, maybeToList)

data ListZipper a = ListZipper
  { before :: [a]
  , after  :: [a]
  }

withListZipper :: (ListZipper a -> ListZipper b) -> [a] -> [b]
withListZipper f = close . f . open
  where
    open :: [a] -> ListZipper a
    open = ListZipper []

    close :: ListZipper a -> [a]
    close (ListZipper b a) = reverse b ++ a

next :: ListZipper a -> ListZipper a
next (ListZipper b a) = case a of
  locus : after -> ListZipper (locus : b) after
  _             -> ListZipper b a

here :: ListZipper a -> Maybe a
here (ListZipper _ a) = listToMaybe a

-- | Navigate to next point that succeeds (or to the end).
--
find :: (a -> Bool) -> ListZipper a -> ListZipper a
find prop = go
  where
    go lz = case here lz of
      Nothing -> lz
      Just (prop -> True) -> lz
      _ -> go (next lz)

-- | Like `Data.Map.alter`, but for lists.
--
atLocus :: (Maybe a -> Maybe a) -> ListZipper a -> ListZipper a
atLocus f (ListZipper b a) = case a of
  locus : after -> ListZipper b (maybeToList (f (Just locus)) ++ after)
  _             -> ListZipper b (maybeToList (f Nothing))
