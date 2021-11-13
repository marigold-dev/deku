module Test.Capabilities.SelectionRange (unit_selection_range) where

-- TODO: There is a bug in the lsp-test library that is preventing it from
-- parsing the response to this request from the server properly, causing this test to fail

--import Language.LSP.Test
--import Language.LSP.Types
--import Language.LSP.Types qualified as LSP (Range (..))
--import Range
--import Data.Function ((&))
--import Test.Common.Util (readContract)
--import Control.Lens ((^.))
--import AST hiding (List)
--import Language.LSP.Types.Lens (range)
--import System.FilePath ((</>))

import Test.HUnit (Assertion)

--import Test.Common.Util (getResponseResult, openLigoDoc, runHandlersTest)
--import Test.Common.Capabilities.Util qualified as Common (contractsDir)
--import Test.Common.FixedExpectations (shouldBe)

unit_selection_range :: Assertion
unit_selection_range = pure ()

--contractsDir :: FilePath
--contractsDir = Common.contractsDir </> "selection-range"
--
---- lsp-test doesn't provide a function for testing selection range
--getSelectionRanges :: TextDocumentIdentifier -> [Position] -> Session [SelectionRange]
--getSelectionRanges doc positions =
--  let params = SelectionRangeParams Nothing Nothing doc (List positions)
--  in (\(List x) -> x) <$> getResponseResult <$> request STextDocumentSelectionRange params

--unit_selection_range = do
--  let filename = "heap.ligo"
--
--  selectionRanges <- runHandlersTest contractsDir $ do
--    doc <- openLigoDoc filename
--    getSelectionRanges doc [Position 15 7]
--  fmap (^. range) selectionRanges `shouldBe`
--    [ LSP.Range (Position 16 8) (Position 16 12)
--    , LSP.Range (Position 16 8) (Position 16 16)
--    , LSP.Range (Position 16 8) (Position 16 21)
--    , LSP.Range (Position 15 6) (Position 18  9)
--    , LSP.Range (Position 14 4) (Position 18  9)
--    , LSP.Range (Position 11 3) (Position 21  4)
--    , LSP.Range (Position 11 3) (Position 21 11)
--    , LSP.Range (Position 10 1) (Position 21 11)
--    , LSP.Range (Position  1 1) (Position 105  1)
--    ]
