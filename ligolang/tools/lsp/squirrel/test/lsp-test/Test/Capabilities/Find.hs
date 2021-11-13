module Test.Capabilities.Find
  ( test_findDefinitionAndGoToReferencesCorrespondence
  , test_heapLeftLocal
  , unit_definitionOfId
  , unit_definitionOfLeft
  , unit_referenceOfId
  , unit_referenceOfLeft
  , unit_definitionOfXInWildcard
  , unit_referenceOfXInWildcard

  , unit_type_of_heap_arg
  , unit_type_of_heap_const
  , unit_type_of_let
  , unit_type_of_pascaligo_lambda_arg
  , unit_pascaligo_local_type
  ) where

import System.FilePath ((</>))

import AST.Scope (Fallback)

import Test.Common.Capabilities.Find
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion)

import Range (interval)

test_findDefinitionAndGoToReferencesCorrespondence :: TestTree
test_findDefinitionAndGoToReferencesCorrespondence =
  findDefinitionAndGoToReferencesCorrespondence @Fallback invariants

-- TODO: See the note for `left, local` in common Find test.
test_heapLeftLocal :: TestTree
test_heapLeftLocal =
  findDefinitionAndGoToReferencesCorrespondence @Fallback [test]
  where
    test = DefinitionReferenceInvariant
      { driFile = contractsDir </> "heap.ligo"
      , driDesc = "left, local"
      , driDef = Just (interval 77 9 13)
      , driRefs = [ interval 99 15 19
                  , interval 90 13 17
                  , interval 89 30 34
                  , interval 87 22 26
                  , interval 86 36 40
                  , interval 85 10 14
                  , interval 84 16 20
                  , interval 83 7 11
                  ]
      }

unit_definitionOfId :: Assertion
unit_definitionOfId = definitionOfId @Fallback

unit_referenceOfId :: Assertion
unit_referenceOfId = referenceOfId @Fallback

unit_definitionOfLeft :: Assertion
unit_definitionOfLeft = definitionOfLeft @Fallback

unit_referenceOfLeft :: Assertion
unit_referenceOfLeft = referenceOfLeft @Fallback

unit_definitionOfXInWildcard :: Assertion
unit_definitionOfXInWildcard = definitionOfXInWildcard @Fallback

unit_referenceOfXInWildcard :: Assertion
unit_referenceOfXInWildcard = referenceOfXInWildcard @Fallback

unit_type_of_heap_const :: Assertion
unit_type_of_heap_const = typeOfHeapConst @Fallback

unit_type_of_heap_arg :: Assertion
unit_type_of_heap_arg = typeOfHeapArg @Fallback

unit_type_of_let :: Assertion
unit_type_of_let = typeOfLet @Fallback

unit_type_of_pascaligo_lambda_arg :: Assertion
unit_type_of_pascaligo_lambda_arg = typeOfPascaligoLambdaArg @Fallback

unit_pascaligo_local_type :: Assertion
unit_pascaligo_local_type = pascaligoLocalType @Fallback

-- See LIGO-110
-- unit_type_of_camligo_lambda_arg :: Assertion
-- unit_type_of_camligo_lambda_arg = typeOfCamligoLambdaArg @Fallback
