module Test.Common.Capabilities.Hover
  ( contractsDir
  , HoverTest (..)
  , hover'
  , checkHover
  , unit_hover
  ) where

import Prelude hiding (lines)

import Data.List (find)
import Data.Text (Text, lines)
import Control.Monad (unless)
import Language.LSP.Types (Hover (..), HoverContents (..), MarkupContent (..))
import System.Directory (makeAbsolute)
import System.FilePath ((</>))

import Test.HUnit (Assertion)

import AST.Capabilities.Hover (hoverDecl)
import AST.Pretty (docToText, ppToText)
import AST.Scope (HasScopeForest)
import AST.Scope.ScopedDecl (Type (..), lppLigoLike)
import AST.Skeleton (Lang (..))

import Range (Range (..), interval, point)

import Test.Common.Capabilities.Util qualified as Common (contractsDir)
import Test.Common.FixedExpectations (expectationFailure, shouldBe)
import Test.Common.Util (readContractWithScopes)

contractsDir :: FilePath
contractsDir = Common.contractsDir </> "hover"

data HoverTest = HoverTest
  { htDefinition :: Range
  , htName :: Text
  , htType :: Type
  , htDoc :: [Text]
  , htDialect :: Lang
  }

hover' :: Range -> Text -> Type -> Lang -> HoverTest
hover' definition name type' = HoverTest definition name type' []

checkHover :: forall parser. HasScopeForest parser IO => FilePath -> Range -> HoverTest -> Assertion
checkHover fp reference HoverTest{..} = do
  contract <- readContractWithScopes @parser fp
  case hoverDecl reference contract of
    Nothing -> expectationFailure "Expected a hover definition, but got Nothing"
    Just (Hover (HoverContents (MarkupContent _ (lines -> (ty : _ : def : doc)))) _) -> do
      ty `shouldBe` (htName <> " : " <> docToText (lppLigoLike htDialect htType))
      def `shouldBe` ("*defined at* " <> ppToText htDefinition)
      case doc of
        [] -> unless (null htDoc) $ expectationFailure "Expected no documentation, but got some"
        _  -> Just (ppToText htDoc) `shouldBe` find (/= "") doc
    _ -> expectationFailure "Hover definition is not of the expected type"

unit_hover :: forall parser. HasScopeForest parser IO => Assertion
unit_hover = do
  fp <- makeAbsolute $ contractsDir </> "apply-type.ligo"
  let type' = ApplyType (AliasType "contract") [AliasType "unit"]
  checkHover @parser fp (point 3 23){_rFile = fp} (hover' (interval 2 9 10){_rFile = fp} "c" type' Pascal)
