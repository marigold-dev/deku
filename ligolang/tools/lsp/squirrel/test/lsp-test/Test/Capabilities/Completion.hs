module Test.Capabilities.Completion
  ( test_completion
  ) where

import AST.Scope (Fallback)

import Test.Common.Capabilities.Completion
import Test.Tasty (TestTree)

test_completion :: IO TestTree
test_completion = completionDriver @Fallback caseInfos
