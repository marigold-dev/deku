module Test.Parsers
  ( test_okayContracts
  , test_badContracts
  ) where

import System.FilePath ((</>))

import AST (Fallback)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Test.Common.Util (getContractsWithExtension, supportedExtensions)
import Test.Common.Util.Parsers (checkFile)

-- Since preprocessors are dealt by `ligo preprocess`, we ignore these contracts
-- since we can't parse them here.
okayIgnoreContracts :: [FilePath]
okayIgnoreContracts = map ("test/contracts/bugs" </>) ignore
  where
    ignore = ["LIGO-185.ligo", "LIGO-185.mligo", "LIGO-185.religo"]

okayContractsDirs :: [FilePath]
okayContractsDirs = ["test/contracts/bugs"]

-- Since preprocessors are dealt by `ligo preprocess`, we ignore these contracts
-- since their errors would only be thrown by `ligo preprocess`.
badIgnoreContracts :: [FilePath]
badIgnoreContracts = map ("test/contracts/bad" </>) ignore
  where
    ignore = ["extra-line-marker.mligo", "LIGO-105.ligo", "LIGO-105.mligo", "LIGO-105.religo"]

badContractsDirs :: [FilePath]
badContractsDirs = ["test/contracts/bad"]

getOkayContractsWithExtension :: String -> IO [FilePath]
getOkayContractsWithExtension ext =
  foldMap (getContractsWithExtension ext okayIgnoreContracts) okayContractsDirs

getBadContractsWithExtension :: String -> IO [FilePath]
getBadContractsWithExtension ext
  = foldMap (getContractsWithExtension ext badIgnoreContracts) badContractsDirs

getOkayContracts :: IO [FilePath]
getOkayContracts = foldMap getOkayContractsWithExtension supportedExtensions

getBadContracts :: IO [FilePath]
getBadContracts = foldMap getBadContractsWithExtension supportedExtensions

test_okayContracts :: IO TestTree
test_okayContracts
  = testGroup "Parsers should parse these contracts" <$> testCases
  where
    testCases = map makeTestCase <$> getOkayContracts
    makeTestCase contractPath = testCase contractPath (checkFile @Fallback True contractPath)

test_badContracts :: IO TestTree
test_badContracts
  = testGroup "Parsers should not parse these contracts" <$> testCases
  where
    testCases = map makeTestCase <$> getBadContracts
    makeTestCase contractPath = testCase contractPath (checkFile @Fallback False contractPath)
