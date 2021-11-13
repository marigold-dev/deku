module Test.Capabilities.SignatureHelp
  ( test_simpleFunctionCall
  ) where

import AST.Scope (Fallback)

import Test.Common.Capabilities.SignatureHelp
import Test.Tasty (TestTree)

test_simpleFunctionCall :: IO TestTree
test_simpleFunctionCall = simpleFunctionCallDriver @Fallback
