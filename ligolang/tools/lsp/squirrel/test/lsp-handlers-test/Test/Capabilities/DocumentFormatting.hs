module Test.Capabilities.DocumentFormatting (unit_document_formatting) where

import Language.LSP.Test
import Language.LSP.Types
import System.FilePath ((</>))

import Test.HUnit (Assertion)

import Test.Common.Util (openLigoDoc, runHandlersTest)
import Test.Common.Capabilities.Util qualified as Common (contractsDir)
import Test.Common.FixedExpectations (shouldBe)

contractsDir :: FilePath
contractsDir = Common.contractsDir </> "document-format"

unit_document_formatting :: Assertion
unit_document_formatting = do
  let filename = "trailing_space.ligo"
  let expectedFilename = "trailing_space_expected.ligo"
  let formattingOptions = FormattingOptions
        { _tabSize = 2
        , _insertSpaces = True
        , _trimTrailingWhitespace = Just True
        , _insertFinalNewline = Nothing
        , _trimFinalNewlines = Nothing
        }

  (formattedDoc, expectedDoc) <- runHandlersTest contractsDir $ do
    doc <- openLigoDoc filename
    expectedDoc <- openLigoDoc expectedFilename
    formatDoc doc formattingOptions
    (,) <$> documentContents doc <*> documentContents expectedDoc
  formattedDoc `shouldBe` expectedDoc
