module Test.Capabilities.CodeAction.ExtractTypeAlias
  ( test_extractTypeAlias
  ) where

import Test.Tasty

import AST.Scope (Fallback)

import Test.Common.Capabilities.CodeAction.ExtractTypeAlias

test_extractTypeAlias :: TestTree
test_extractTypeAlias = extractTypeAliasDriver fallbackGroup
  where
    fallbackGroup :: TestTree
    fallbackGroup = testGroup "Fallback extraction" (testCases @Fallback)
