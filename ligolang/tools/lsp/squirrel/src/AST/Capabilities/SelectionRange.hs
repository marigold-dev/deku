module AST.Capabilities.SelectionRange
  ( findSelectionRange
  , findCoveringRanges
  ) where

import Language.LSP.Types qualified as J (Position, SelectionRange (..))

import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Duplo.Lattice (leq)
import Duplo.Tree (spineTo)

import AST.Skeleton (LIGO)
import Product (Contains, Product, getElem)
import Range (Range, fromLspPosition, getRange, toLspRange)

-- | Returns a list of ranges covering the given range. The list is
-- ordered from the most nested ranges to the least nested. The ranges
-- correspond to AST nodes.
findCoveringRanges
  :: (Contains Range xs, Eq (Product xs))
  => LIGO xs -> Range -> [Range]
findCoveringRanges tree position = map getRange coveringTrees
  where
    coveringTrees = spineTo (leq position . getElem) tree

-- | Returns a @J.SelectionRange@ covering the given position.
--
-- See
-- https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_selectionRange.
findSelectionRange
  :: (Contains Range xs, Eq (Product xs))
  => LIGO xs -> J.Position -> J.SelectionRange
findSelectionRange tree (fromLspPosition -> startingRange)
  = findCoveringRanges tree startingRange
  & makeSelectionRange
  & fromMaybe defaultRange
  where
    defaultRange = J.SelectionRange
      { _range = toLspRange startingRange
      , _parent = Nothing
      }

makeSelectionRange :: [Range] -> Maybe J.SelectionRange
makeSelectionRange = foldr folder Nothing
  where
    folder range parent = Just J.SelectionRange
      { _range = toLspRange range
      , _parent = parent
      }
