module Test.Capabilities.Rename
  ( unit_rename_example_fail
  , unit_rename_example_id
  , unit_rename_example_param
  , unit_rename_example_in_included_file
  ) where

import Control.Lens hiding ((:>))
import Data.Text (Text)
import System.Directory (makeAbsolute)
import System.FilePath ((</>))

import Data.HashMap.Strict qualified as HashMap

import Language.LSP.Test
import Language.LSP.Types hiding (Range (..))
import Language.LSP.Types qualified as LSP
import Language.LSP.Types.Lens qualified as LSP
import Test.HUnit (Assertion)

import qualified Test.Common.Capabilities.Util as Common (contractsDir)
import Test.Common.FixedExpectations (expectationFailure, shouldBe, shouldThrow, anyException)
import Test.Common.Util (getResponseResult, openLigoDoc, runHandlersTest)

contractsDir :: FilePath
contractsDir = Common.contractsDir </> "rename"

-- lsp-test doesn't provide a (good) function for testing rename
getRename :: TextDocumentIdentifier -> Position -> Text -> Session WorkspaceEdit
getRename doc pos newName =
  let params = RenameParams doc pos Nothing newName
  in getResponseResult <$> request STextDocumentRename params

testRename
  :: FilePath   -- ^ File path
  -> Position   -- ^ Location of identifier to rename
  -> Text       -- ^ New name
  -> [(FilePath, [LSP.Range])] -- ^ List of edits
  -> Assertion
testRename fp pos newName edits = do
  workspaceEdit <- runHandlersTest contractsDir $ do
    doc <- openLigoDoc fp
    getRename doc pos newName
  case workspaceEdit ^. LSP.changes of
    Nothing -> expectationFailure "should be able to rename"
    Just weMap -> weMap `shouldBe` toWorkspaceEditMap newName edits

toWorkspaceEditMap :: Text -> [(FilePath, [LSP.Range])] -> WorkspaceEditMap
toWorkspaceEditMap newName = HashMap.fromList . fmap toKeyValue
  where
    toKeyValue :: (FilePath, [LSP.Range]) -> (Uri, List TextEdit)
    toKeyValue (fp, edits) = (filePathToUri fp, LSP.List $ fmap (`TextEdit` newName) edits)

testRenameFail :: FilePath -> Position -> Text -> Assertion
testRenameFail fp pos newName = do
  workspaceEdit <- runHandlersTest contractsDir $ do
    doc <- openLigoDoc fp
    getRename doc pos newName
  case workspaceEdit ^. LSP.changes of
    Just _  -> expectationFailure "should not be able to edit"
    Nothing -> pure ()

unit_rename_example_fail :: Assertion
unit_rename_example_fail = do
  fp <- makeAbsolute (contractsDir </> "id.ligo")
  testRenameFail fp (Position 0 15) "new_const" `shouldThrow` anyException

unit_rename_example_id :: Assertion
unit_rename_example_id = do
  fp <- makeAbsolute (contractsDir </> "id.ligo")
  testRename fp (Position 0 10) "new_id" [(fp, [LSP.Range (Position 0 9) (Position 0 11)])]

unit_rename_example_param :: Assertion
unit_rename_example_param = do
  fp <- makeAbsolute (contractsDir </> "params.mligo")
  testRename fp (Position 2 10) "aa"
    [(fp, [ LSP.Range (Position 2 35) (Position 2 36)
          , LSP.Range (Position 2 10) (Position 2 11)])]

unit_rename_example_in_included_file :: Assertion
unit_rename_example_in_included_file = do
  fp1 <- makeAbsolute (contractsDir </> "LIGO-104-A1.mligo")
  fp2 <- makeAbsolute (contractsDir </> "LIGO-104-A2.mligo")
  testRename fp2 (Position 0 4) "renamed"
    [ (fp1, [LSP.Range (Position 2 10) (Position 2 19)])
    , (fp2, [LSP.Range (Position 0 4) (Position 0 13)])
    ]
