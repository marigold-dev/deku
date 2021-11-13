module Test.Capabilities.Hover ( unit_hover ) where

import Data.Text (isPrefixOf)
import Language.LSP.Test
import Language.LSP.Types (Hover (..), HoverContents (..), MarkupContent (..), Position (..))

import Test.HUnit (Assertion)

import Test.Common.Capabilities.Hover (contractsDir)
import Test.Common.FixedExpectations (shouldSatisfy)
import Test.Common.Util (openLigoDoc, runHandlersTest)

unit_hover :: Assertion
unit_hover = do
  maybeHover <- runHandlersTest contractsDir $ do
    doc <- openLigoDoc "eq_bool.ligo"
    getHover doc (Position 3 11)
  shouldSatisfy maybeHover $ \case
    Just (Hover (HoverContents (MarkupContent _ hoverText)) _) ->
      "b : bool" `isPrefixOf` hoverText
    _ -> error "expected a hover"
