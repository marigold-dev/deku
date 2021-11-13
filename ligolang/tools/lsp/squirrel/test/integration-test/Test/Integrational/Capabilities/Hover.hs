module Test.Integrational.Capabilities.Hover
  ( unit_hover
  ) where

import Test.HUnit (Assertion)

import AST.Scope (Standard)

import Test.Common.Capabilities.Hover qualified as Hover (unit_hover)

unit_hover :: Assertion
unit_hover = Hover.unit_hover @Standard
