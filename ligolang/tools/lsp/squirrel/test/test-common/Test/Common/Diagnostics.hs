module Test.Common.Diagnostics
  ( inputFile
  , parseDiagnosticsDriver
  ) where

import Data.Text (Text)
import Parser
import Range
import System.FilePath ((</>))

import AST.Parser (collectAllErrors, parseContractsWithDependencies, parsePreprocessed)
import AST.Scope (HasScopeForest, addScopes, lookupContract)
import AST.Skeleton (Error (..))

import qualified Test.Common.Capabilities.Util as Util (contractsDir)
import Test.Common.FixedExpectations (expectationFailure, shouldMatchList)
import Test.Tasty.HUnit (Assertion)

inputDir :: FilePath
inputDir = Util.contractsDir </> "diagnostic"

inputFile :: FilePath
inputFile = inputDir </> "a.mligo"

simplifyError :: Msg -> (Range, Text)
simplifyError (range, Error t _) = (range, t)

-- Try to parse a file, and check that the proper error messages are generated
parseDiagnosticsDriver
  :: forall impl. HasScopeForest impl IO
  => [(Range, Text)]
  -> Assertion
parseDiagnosticsDriver expectedMsgs = do
  parsedContracts <- parseContractsWithDependencies parsePreprocessed inputDir
  contractGraph <- addScopes @impl parsedContracts
  let mContract = lookupContract inputFile contractGraph
  case mContract of
    Nothing -> expectationFailure ("Couldn't find " <> inputFile)
    Just contract ->
      let msgs = collectAllErrors contract
       in expectedMsgs `shouldMatchList` fmap simplifyError msgs
