module Test.Scope.ScopedDecl
  ( unit_access_field
  ) where

import Test.Tasty.HUnit (Assertion)

import AST.Scope.ScopedDecl (Type (..), TypeDeclSpecifics (..), TypeField (..), accessField)
import Range (Range, point)

import Test.Common.FixedExpectations (shouldBe)

stubRange :: Range
stubRange = point (-1) (-1)

stubTspec :: TypeDeclSpecifics
stubTspec = TypeDeclSpecifics stubRange (AliasType "stupid-type")

unit_access_field :: Assertion
unit_access_field = do
  let tspec =
        TypeDeclSpecifics stubRange
          (RecordType [TypeField "stupid-name" stubTspec])
      accessor = Right "stupid-name"
  accessField tspec accessor `shouldBe` Just stubTspec
