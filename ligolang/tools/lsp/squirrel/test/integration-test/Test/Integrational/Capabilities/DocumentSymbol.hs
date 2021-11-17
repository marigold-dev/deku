module Test.Integrational.Capabilities.DocumentSymbol
  ( unit_document_symbols_example_heap
  , unit_document_symbols_example_access
  , unit_document_symbols_example_let_camligo
  , unit_document_symbols_example_let_religo
  ) where

import AST.Scope (Standard)

import Test.Common.Capabilities.DocumentSymbol
import Test.HUnit (Assertion)

unit_document_symbols_example_heap :: Assertion
unit_document_symbols_example_heap = documentSymbolsExampleHeapDriver @Standard

unit_document_symbols_example_access :: Assertion
unit_document_symbols_example_access = documentSymbolsExampleAccessDriver @Standard

unit_document_symbols_example_let_camligo :: Assertion
unit_document_symbols_example_let_camligo = documentSymbolsExampleLetCamligoDriver @Standard

unit_document_symbols_example_let_religo :: Assertion
unit_document_symbols_example_let_religo = documentSymbolsExampleLetReligoDriver @Standard
