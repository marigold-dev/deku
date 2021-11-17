module Test.Common.Capabilities.DocumentSymbol
  ( documentSymbolsExampleHeapDriver
  , documentSymbolsExampleAccessDriver
  , documentSymbolsExampleLetCamligoDriver
  , documentSymbolsExampleLetReligoDriver
  ) where

import Control.Lens ((^.))
import Data.Text (Text)
import Language.LSP.Types
  (Location (Location, _range), SymbolInformation (SymbolInformation, _kind, _location, _name),
  SymbolKind (..), Uri (Uri))
import Language.LSP.Types.Lens (character, end, line, start)
import System.FilePath ((</>))

import Test.HUnit (Assertion)

import AST.Capabilities.DocumentSymbol (extractDocumentSymbols)
import AST.Scope (HasScopeForest)

import Test.Common.Capabilities.Util qualified as Common (contractsDir)
import Test.Common.FixedExpectations (shouldBe)
import Test.Common.Util (readContractWithScopes)

contractsDir :: FilePath
contractsDir = Common.contractsDir </> "document-symbol"

type SimpleSymInfo = (Text, SymbolKind, (Int, Int), (Int, Int))

simplify :: SymbolInformation -> SimpleSymInfo
simplify SymbolInformation{_name, _kind, _location = Location{_range}} =
  ( _name
  , _kind
  , (_range ^. start . line, _range ^. start . character)
  , (_range ^. end . line, _range ^. end . character)
  )

documentSymbolsExampleHeapDriver :: forall impl. HasScopeForest impl IO => Assertion
documentSymbolsExampleHeapDriver = do
  tree <- readContractWithScopes @impl (contractsDir </> "heap.ligo")
  symbols <- extractDocumentSymbols (Uri "<test>") tree
  map simplify symbols `shouldBe`
    [ ("heap", SkTypeParameter, (3, 5), (3, 9))
    , ("is_empty", SkFunction, (5,9), (5,17))
    , ("get_top", SkFunction, (7, 9), (7, 16))
    , ("pop_switch", SkFunction, (9, 9), (9, 19))
    , ("pop_", SkFunction, (22, 9), (22, 13))
    , ("insert", SkFunction, (46, 9), (46, 15))
    , ("pop", SkFunction, (66, 9), (66, 12))
    , ("const empty",SkConstant,(105,6),(105,11))
    ]

documentSymbolsExampleAccessDriver :: forall impl. HasScopeForest impl IO => Assertion
documentSymbolsExampleAccessDriver = do
  tree <- readContractWithScopes @impl (contractsDir </> "access.ligo")
  symbols <- extractDocumentSymbols (Uri "<test>") tree
  map simplify symbols `shouldBe`
    [ ("const owner", SkConstant , (2, 6), (2, 11))
    , ("main", SkFunction, (4,9), (4,13))
    ]

documentSymbolsExampleLetCamligoDriver :: forall impl. HasScopeForest impl IO => Assertion
documentSymbolsExampleLetCamligoDriver = do
  tree <- readContractWithScopes @impl (contractsDir </> "let.mligo")
  symbols <- extractDocumentSymbols (Uri "<test>") tree
  map simplify symbols `shouldBe`
    [ ("const a", SkConstant, (0, 4), (0, 5))

    , ("const b", SkConstant, (1, 5), (1, 6))
    , ("const c", SkConstant, (1, 8), (1, 9))

    , ("const d", SkConstant, (2, 5), (2, 6))
    , ("const e", SkConstant, (2, 9), (2, 10))

    , ("const f", SkConstant, (3, 5), (3, 6))
    , ("const h", SkConstant, (3, 13), (3, 14))

    , ("const j", SkConstant, (4, 10), (4, 11))
    , ("const k", SkConstant, (4, 13), (4, 14))
    , ("const m", SkConstant, (4, 23), (4, 24))
    , ("const n", SkConstant, (4, 26), (4, 27))
    ]

documentSymbolsExampleLetReligoDriver :: forall impl. HasScopeForest impl IO => Assertion
documentSymbolsExampleLetReligoDriver = do
  tree <- readContractWithScopes @impl (contractsDir </> "let.religo")
  symbols <- extractDocumentSymbols (Uri "<test>") tree
  map simplify symbols `shouldBe`
    [ ("const a", SkConstant, (0, 4), (0, 5))

    , ("const b", SkConstant, (1, 5), (1, 6))
    , ("const c", SkConstant, (1, 8), (1, 9))

    , ("const d", SkConstant, (2, 6), (2, 7))
    , ("const e", SkConstant, (2, 16), (2, 17))

    , ("const f", SkConstant, (3, 6), (3, 7))
    , ("const h", SkConstant, (3, 14), (3, 15))
    ]
