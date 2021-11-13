{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Integrational.Cli
  ( test_ligo_159
  ) where

import Control.Exception.Safe (SomeException, fromException, tryJust)
import Data.Foldable (asum)
import System.FilePath ((</>))

import Cli
import ParseTree (Source (..))

import Test.Common.FixedExpectations (HasCallStack, expectationFailure)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

filterException :: SomeException -> Maybe SomeLigoException
filterException e = asum
  [ SomeLigoException <$> fromException @LigoDecodedExpectedClientFailureException e
  , SomeLigoException <$> fromException @LigoUnexpectedCrashException              e
  ]

checkFile :: HasCallStack => FilePath -> TestTree
checkFile path = testCase path $
  tryJust filterException (getLigoDefinitions $ Path path) >>= \case
    Left  _ -> pure ()
    Right _ -> expectationFailure "Expected contract to fail, but it has succeeded."

test_ligo_159 :: TestTree
test_ligo_159 = testGroup "Contracts should throw errors" $ checkFile <$> files
  where
    files :: [FilePath]
    files = (\f -> "test" </> "contracts" </> "json-bugs" </> f) <$>
      [ "LIGO-159_2.mligo"
      , "LIGO-159_3.mligo"
      ]
