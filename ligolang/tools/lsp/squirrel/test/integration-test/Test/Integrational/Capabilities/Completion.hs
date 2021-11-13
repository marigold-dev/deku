module Test.Integrational.Capabilities.Completion
  ( test_completion
  ) where

import AST.Scope (Standard)

import Test.Common.Capabilities.Completion
import Test.Tasty (TestTree)

test_completion :: IO TestTree
test_completion = completionDriver @Standard caseInfos
