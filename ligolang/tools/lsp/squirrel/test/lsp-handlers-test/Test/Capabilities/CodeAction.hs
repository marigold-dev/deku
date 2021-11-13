module Test.Capabilities.CodeAction (unit_code_action) where

import Language.LSP.Test
import Language.LSP.Types
import System.FilePath ((</>))

import Test.HUnit (Assertion)

import Test.Common.Util (openLigoDoc, runHandlersTest)
import Test.Common.Capabilities.CodeAction.ExtractTypeAlias (extractTextEdits, testInfos, constructExpectedWorkspaceEdit, TestInfo (..))
import Test.Common.Capabilities.Util qualified as Common (contractsDir)
import Test.Common.FixedExpectations (shouldBe)

contractsDir :: FilePath
contractsDir = Common.contractsDir </> "code-action" </> "extract-type-definition"

toCodeAction :: Command |? CodeAction -> CodeAction
toCodeAction (InL _) = error "expected a CodeAction, not a Command"
toCodeAction (InR action) = action

unit_code_action :: Assertion
unit_code_action = do
  let filename = "simple.ligo"

  codeActions <- runHandlersTest contractsDir $ do
    doc <- openLigoDoc filename
    getCodeActions doc (Range (Position 1 23) (Position 1 23))

  length codeActions `shouldBe` 1
  let textEdits = fmap (extractTextEdits . toCodeAction) codeActions
  textEdits `shouldBe` fmap (constructExpectedWorkspaceEdit . tiExpectedEdits) testInfos
