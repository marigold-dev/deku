module Test.Capabilities.PrepareRename
  ( unit_prepare_rename_example_fail
  , unit_prepare_rename_example_id
  , unit_prepare_rename_example_param
  , unit_prepare_rename_example_in_included_file
  ) where

import System.Directory (makeAbsolute)
import System.FilePath ((</>))

import Language.LSP.Test
import Language.LSP.Types hiding (Range (..))
import Language.LSP.Types qualified as LSP
import Test.HUnit (Assertion)

import qualified Test.Common.Capabilities.Util as Common (contractsDir)
import Test.Common.FixedExpectations (expectationFailure, shouldBe)
import Test.Common.Util (getResponseResult, openLigoDoc, runHandlersTest)

contractsDir :: FilePath
contractsDir = Common.contractsDir </> "rename"

-- lsp-test doesn't provide a function for testing prepare rename
getPrepareRename :: TextDocumentIdentifier -> Position -> Session (Maybe (LSP.Range |? RangeWithPlaceholder))
getPrepareRename doc pos =
  let params = PrepareRenameParams doc pos
  in getResponseResult <$> request STextDocumentPrepareRename params

testPrepareRename
  :: FilePath   -- ^ File path
  -> Position   -- ^ Rename location
  -> LSP.Range  -- ^ Expected declaration position
  -> Assertion
testPrepareRename fp pos range = do
  r <- runHandlersTest contractsDir $ do
    doc <- openLigoDoc fp
    getPrepareRename doc pos
  case r of
    Just (LSP.InL range') -> range' `shouldBe` range
    Just (LSP.InR _) -> expectationFailure "should not receive RangeWithPlaceholder"
    Nothing -> expectationFailure "should be able to rename"

testPrepareRenameFail :: FilePath -> Position -> Assertion
testPrepareRenameFail fp pos = do
  r <- runHandlersTest contractsDir $ do
    doc <- openLigoDoc fp
    getPrepareRename doc pos
  case r of
    Just (LSP.InL _) -> expectationFailure "should be able to rename"
    Just (LSP.InR _) -> expectationFailure "should not receive RangeWithPlaceholder"
    Nothing -> pure ()

unit_prepare_rename_example_fail :: Assertion
unit_prepare_rename_example_fail = do
  fp <- makeAbsolute (contractsDir </> "id.ligo")
  testPrepareRenameFail fp (Position 0 15)

unit_prepare_rename_example_id :: Assertion
unit_prepare_rename_example_id = do
  fp <- makeAbsolute (contractsDir </> "id.ligo")
  testPrepareRename fp (Position 0 10) (LSP.Range (Position 0 9) (Position 0 11))

unit_prepare_rename_example_param :: Assertion
unit_prepare_rename_example_param = do
  fp <- makeAbsolute (contractsDir </> "params.mligo")
  testPrepareRename fp (Position 2 10) (LSP.Range (Position 2 10) (Position 2 11))

unit_prepare_rename_example_in_included_file :: Assertion
unit_prepare_rename_example_in_included_file = do
  fp <- makeAbsolute (contractsDir </> "LIGO-104-A2.mligo")
  testPrepareRename fp (Position 0 4) (LSP.Range (Position 0 4) (Position 0 13))
