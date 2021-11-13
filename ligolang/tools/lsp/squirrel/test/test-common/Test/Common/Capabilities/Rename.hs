module Test.Common.Capabilities.Rename
  ( renameFail
  , renameId
  , renameParam
  , renameInIncludedFile
  , renameNestedInclude
  ) where

import Control.Arrow ((***))
import Data.HashMap.Strict qualified as HM
import Data.Text (Text)
import Data.Text qualified as T
import Language.LSP.Types qualified as J
import System.Directory (makeAbsolute)
import System.FilePath ((</>))
import Test.HUnit (Assertion)

import AST.Capabilities.Rename (RenameDeclarationResult (NotFound, Ok), prepareRenameDeclarationAt, renameDeclarationAt)
import AST.Scope (HasScopeForest)
import Range (Range (..), toLspRange, interval, point)

import Test.Common.Capabilities.Util qualified as Common (contractsDir)
import Test.Common.FixedExpectations (expectationFailure, shouldBe)
import Test.Common.Util (readContractWithScopes)

contractsDir :: FilePath
contractsDir = Common.contractsDir </> "rename"

testRenameOk
  :: forall impl. HasScopeForest impl IO
  => Range  -- ^ Rename location
  -> Text  -- ^ Expected old name
  -> Range  -- ^ Expected declaration position
  -> Text  -- ^ New name
  -> [(FilePath, [Range])]  -- ^ Expected map with edits
  -> Assertion
testRenameOk pos name (Range (declLine, declCol, _) _ declFile) newName expected = do
    let fp = _rFile pos
    tree <- readContractWithScopes @impl fp

    let expected' =
          HM.fromList
          $ map (J.filePathToUri *** J.List . map (flip J.TextEdit newName . toLspRange)) expected

    case prepareRenameDeclarationAt pos tree of
      Nothing -> expectationFailure "Should be able to rename"
      Just decl -> do
        _rFile decl `shouldBe` declFile
        toLspRange decl `shouldBe`
          J.Range
            (J.Position (declLine - 1) (declCol - 1))
            (J.Position (declLine - 1) (declCol + len - 1))

    case renameDeclarationAt pos tree newName of
      NotFound -> expectationFailure "Should return edits"
      Ok results -> results `shouldBe` expected'
  where
    len = T.length name

testRenameFail
  :: forall impl. HasScopeForest impl IO
  => FilePath  -- ^ Contract path
  -> (Int, Int)  -- ^ Rename location
  -> Assertion
testRenameFail fp pos = do
    tree <- readContractWithScopes @impl fp

    case prepareRenameDeclarationAt (uncurry point pos) tree of
      Nothing -> pure ()
      Just _ -> expectationFailure "Should not be able to rename"

    case renameDeclarationAt (uncurry point pos) tree "<newName>" of
      NotFound -> pure ()
      Ok _ -> expectationFailure "Should not return edits"

renameFail :: forall impl. HasScopeForest impl IO => Assertion
renameFail =
  testRenameFail @impl (contractsDir </> "id.ligo") (1, 16)

renameId :: forall impl. HasScopeForest impl IO => Assertion
renameId = do
  fp <- makeAbsolute (contractsDir </> "id.ligo")
  testRenameOk @impl (point 1 11){_rFile = fp} "id" (point 1 10){_rFile = fp} "very_id"
    [(fp, [(interval 1 10 12){_rFile = fp}])]

renameParam :: forall impl. HasScopeForest impl IO => Assertion
renameParam = do
  fp <- makeAbsolute (contractsDir </> "params.mligo")
  testRenameOk @impl (point 3 11){_rFile = fp} "a" (point 3 11){_rFile = fp} "aa"
    [(fp, [(interval 3 36 37){_rFile = fp}, (interval 3 11 12){_rFile = fp}])]

renameInIncludedFile :: forall impl. HasScopeForest impl IO => Assertion
renameInIncludedFile = do
  fp1 <- makeAbsolute (contractsDir </> "LIGO-104-A1.mligo")
  fp2 <- makeAbsolute (contractsDir </> "LIGO-104-A2.mligo")
  testRenameOk @impl (point 1 5){_rFile = fp2} "rename_me" (point 1 5){_rFile = fp2} "renamed"
    [(fp1, [(interval 3 11 20){_rFile = fp1}]), (fp2, [(interval 1 5 14){_rFile = fp2}])]

-- Regression test for LIGO-260
renameNestedInclude :: forall impl. HasScopeForest impl IO => Assertion
renameNestedInclude = do
  func  <- makeAbsolute (contractsDir </> "LIGO-260" </> "Func.mligo")
  param <- makeAbsolute (contractsDir </> "LIGO-260" </> "Param.mligo")
  let def = (interval 1 5 6){_rFile = func}
  testRenameOk @impl def "f" def "func"
    [ (param, [(interval 1 2 3){_rFile = param}])
    , (func, [(interval 4 3 4){_rFile = func}, def])
    ]
