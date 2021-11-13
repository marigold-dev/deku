module Test.Diagnostics
  ( unit_bad_parse
  ) where

import Data.Text (Text)

import AST.Scope (Fallback)
import Range (Range (..))

import Test.Common.Diagnostics (inputFile, parseDiagnosticsDriver)
import Test.Tasty.HUnit (Assertion)

expectedMsgs :: [(Range, Text)]
expectedMsgs =
  [ (mkRange (3, 17) (3, 23), "Unexpected: :: int")
  , (mkRange (3, 17) (3, 23), ":: int")
  , (mkRange (3, 20) (3, 23), "int")
  ]
  where
    mkRange :: (Int, Int) -> (Int, Int) -> Range
    mkRange (a, b) (c, d) = Range (a, b, 0) (c, d, 0) inputFile

-- Try to parse a file, and check that the proper error messages are generated
unit_bad_parse :: Assertion
unit_bad_parse = parseDiagnosticsDriver @Fallback expectedMsgs
