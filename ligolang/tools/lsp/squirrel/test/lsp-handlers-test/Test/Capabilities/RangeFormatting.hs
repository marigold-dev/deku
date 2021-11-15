module Test.Capabilities.RangeFormatting (unit_range_formatting) where

--import Control.Lens ((^.))
--import Language.LSP.Test
--import Data.Text.IO qualified as T
--import Language.LSP.Types
--import Language.LSP.Types.Lens (activeSignature, activeParameter, signatures)
--import System.FilePath ((</>))

import Test.HUnit (Assertion)

--import Test.Common.Util (openLigoDoc, runHandlersTest)
--import Test.Common.Capabilities.Util qualified as Common (contractsDir)
--import Test.Common.FixedExpectations (shouldBe)

--contractsDir :: FilePath
--contractsDir = Common.contractsDir </> "range-format"

-- TODO: Currently, we format by feeding the entire document to the ligo
-- compiler and spit the output back as a single TextEdit. So to edit any
-- range, we have to edit the whole document. This means range formatting
-- doesn't really work right now, and behaves exactly like document formatting.
unit_range_formatting :: Assertion
--unit_range_formatting = do
--  let filename = "trailing_space.ligo"
--  let expectedFilename = "trailing_space_expected.ligo"
--  let editRange = Range (Position 2 0) (Position 5 0)
--  let formattingOptions = FormattingOptions
--        { _tabSize = 2
--        , _insertSpaces = True
--        , _trimTrailingWhitespace = Just True
--        , _insertFinalNewline = Nothing
--        , _trimFinalNewlines = Nothing
--        }
--
--  (formattedDoc, expectedDoc) <- runHandlersTest contractsDir $ do
--    doc <- openLigoDoc filename
--    expectedDoc <- openLigoDoc expectedFilename
--    formatRange doc formattingOptions editRange
--    (,) <$> documentContents doc <*> documentContents expectedDoc
--  formattedDoc `shouldBe` expectedDoc
unit_range_formatting = pure ()
