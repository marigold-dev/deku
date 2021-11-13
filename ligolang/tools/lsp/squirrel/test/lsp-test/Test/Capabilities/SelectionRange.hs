module Test.Capabilities.SelectionRange
  ( unit_selectionRangeInsideCase
  ) where

import Control.Lens ((^.))
import Data.Function ((&))
import System.FilePath ((</>))
import Test.HUnit (Assertion)

import AST.Capabilities.SelectionRange (findCoveringRanges)
import AST.Skeleton (nestedLIGO)
import Range (Range (..), point)

import Test.Common.Capabilities.Util qualified as Common (contractsDir)
import Test.Common.FixedExpectations (shouldBe)
import Test.Common.Util (readContract)

contractsDir :: FilePath
contractsDir = Common.contractsDir </> "selection-range"

data SimpleRange = SimpleRange (Int, Int) (Int, Int) FilePath
  deriving stock (Eq, Show)

simplify :: Range -> SimpleRange
simplify (Range (l1, c1, _) (l2, c2, _) f) = SimpleRange (l1, c1) (l2, c2) f

unit_selectionRangeInsideCase :: Assertion
unit_selectionRangeInsideCase = do
  let filepath = contractsDir </> "heap.ligo"
  tree <- readContract filepath
  let position = (point 16 8){_rFile = filepath}
      results = findCoveringRanges (tree ^. nestedLIGO) position
              & map simplify
  results `shouldBe` [ SimpleRange (16, 8) ( 16, 12) filepath
                     , SimpleRange (16, 8) ( 16, 16) filepath
                     , SimpleRange (16, 8) ( 16, 21) filepath
                     , SimpleRange (15, 6) ( 18,  9) filepath
                     , SimpleRange (14, 4) ( 18,  9) filepath
                     , SimpleRange (11, 3) ( 21,  4) filepath
                     , SimpleRange (11, 3) ( 21, 11) filepath
                     , SimpleRange (10, 1) ( 21, 11) filepath
                     , SimpleRange ( 1, 1) (105,  1) filepath
                     ]
