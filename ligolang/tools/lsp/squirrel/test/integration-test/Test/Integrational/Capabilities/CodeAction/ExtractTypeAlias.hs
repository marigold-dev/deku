module Test.Integrational.Capabilities.CodeAction.ExtractTypeAlias
  ( test_extractTypeAlias
  ) where

import Test.Tasty

import AST.Scope (Standard)

import Test.Common.Capabilities.CodeAction.ExtractTypeAlias

test_extractTypeAlias :: TestTree
test_extractTypeAlias = extractTypeAliasDriver standardGroup
  where
    standardGroup :: TestTree
    standardGroup = testGroup "Standard extraction" (testCases @Standard)
