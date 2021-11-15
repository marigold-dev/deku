module Test.Capabilities.Definition( unit_definition ) where

import System.Directory (makeAbsolute)
import System.FilePath ((</>))

import Language.LSP.Test
import Language.LSP.Types (Location (..), Position (..), Range (..), filePathToUri, type (|?) (..))
import Test.HUnit (Assertion)

import Test.Common.Capabilities.Util qualified as Common (contractsDir)
import Test.Common.Util (openLigoDoc, runHandlersTest)
import Test.Common.FixedExpectations (shouldBe)

contractsDir :: FilePath
contractsDir = Common.contractsDir </> "definition"

unit_definition :: Assertion
unit_definition = do
  let filename = "increment.ligo"

  eitherDefs <- runHandlersTest contractsDir $ do
    doc <- openLigoDoc filename
    getDefinitions doc (Position 3 3)

  filepath <- makeAbsolute (contractsDir </> filename)
  let uri = filePathToUri filepath
  eitherDefs `shouldBe` InL [Location uri (Range (Position 2 7) (Position 2 13))]
