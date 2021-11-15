module Test.Capabilities.SignatureHelp (unit_signature_help) where

import Control.Lens ((^.))
import Language.LSP.Test
import Language.LSP.Types
import Language.LSP.Types.Lens (activeSignature, activeParameter, signatures)
import System.FilePath ((</>))

import Test.HUnit (Assertion)

import Test.Common.Util (getResponseResult, openLigoDoc, runHandlersTest)
import Test.Common.Capabilities.Util qualified as Common (contractsDir)
import Test.Common.FixedExpectations (shouldBe)

contractsDir :: FilePath
contractsDir = Common.contractsDir </> "signature-help"

-- lsp-test doesn't provide a function for testing signature help
getSignatureHelp :: TextDocumentIdentifier -> Position -> Session SignatureHelp
getSignatureHelp doc pos =
  let params = SignatureHelpParams doc pos Nothing Nothing
  in getResponseResult <$> request STextDocumentSignatureHelp params

unit_signature_help :: Assertion
unit_signature_help = do
  let filename = "all-okay.ligo"

  signatureHelp <- runHandlersTest contractsDir $ do
    doc <- openLigoDoc filename
    getSignatureHelp doc (Position 2 43)
  signatureHelp ^. signatures`shouldBe` List [SignatureInformation
    { _label = "function bar (const i : int)"
    , _documentation = Just (SignatureHelpDocString "")
    , _parameters = Just (List [ParameterInformation {_label = ParameterLabelString "const i : int", _documentation = Nothing}])
    , _activeParameter = Nothing
    }]
  signatureHelp ^. activeSignature `shouldBe` Just 0
  signatureHelp ^. activeParameter `shouldBe` Just 0
