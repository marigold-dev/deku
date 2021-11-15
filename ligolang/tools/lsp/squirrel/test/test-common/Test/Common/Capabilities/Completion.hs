module Test.Common.Capabilities.Completion
  ( completionDriver
  , caseInfos
  ) where

import Data.Maybe (fromJust)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import AST.Capabilities.Completion
import AST.Parser (parseContractsWithDependenciesScopes, parsePreprocessed)
import AST.Scope (HasScopeForest, contractTree, lookupContract)
import Range (point)

import Test.Common.Capabilities.Util qualified (contractsDir)
import Test.Common.FixedExpectations (expectationFailure, shouldMatchList)

contractsDir :: FilePath
contractsDir = Test.Common.Capabilities.Util.contractsDir </> "completion"

data TestInfo = TestInfo
  { tiContract :: FilePath
  , tiPosition :: (Int, Int)
  , tiExpected :: [Completion]
  }

-- Note: Not all completions will be in the completion list for VSCode. This is
-- because VSCode uses a fuzzy matcher, which can't be disabled, which further
-- filters the completion results.
-- See this for a discussion on the matter:
-- https://gitlab.com/serokell/ligo/ligo/-/merge_requests/176#note_679710028
caseInfos :: [TestInfo]
caseInfos =
  [ TestInfo
    { tiContract = "no-prefix.ligo"
    , tiPosition = (1, 46)
    , tiExpected = []
    }
  , TestInfo
    { tiContract = "yes-prefix.ligo"
    , tiPosition = (1, 48)
    , tiExpected =
      [ Completion (NameCompletion "parameter") (TypeCompletion "int") (DocCompletion "")
      , CompletionKeyword (NameCompletion "patch")
      ]
    }

  , TestInfo
    { tiContract = "type-attribute.ligo"
    , tiPosition = (15, 35)
    , tiExpected =
      [ Completion (NameCompletion "id") (TypeCompletion "nat") (DocCompletion "")
      , Completion (NameCompletion "is_admin") (TypeCompletion "bool") (DocCompletion "")
      , CompletionKeyword (NameCompletion "big_map")
      , CompletionKeyword (NameCompletion "if")
      , CompletionKeyword (NameCompletion "begin")
      , CompletionKeyword (NameCompletion "list")
      , CompletionKeyword (NameCompletion "skip")
      , CompletionKeyword (NameCompletion "in")
      , CompletionKeyword (NameCompletion "while")
      , CompletionKeyword (NameCompletion "is")
      , CompletionKeyword (NameCompletion "nil")
      , CompletionKeyword (NameCompletion "recursive")
      , CompletionKeyword (NameCompletion "contains")
      , CompletionKeyword (NameCompletion "function")
      ]
    }
  , TestInfo
    { tiContract = "type-attribute.mligo"
    , tiPosition = (13, 33)
    , tiExpected =
      [ Completion (NameCompletion "id") (TypeCompletion "nat") (DocCompletion "")
      , Completion (NameCompletion "is_admin") (TypeCompletion "bool") (DocCompletion "")
      , CompletionKeyword (NameCompletion "if")
      , CompletionKeyword (NameCompletion "begin")
      , CompletionKeyword (NameCompletion "with")
      , CompletionKeyword (NameCompletion "in")
      ]
    }
  , TestInfo
    { tiContract = "type-attribute.religo"
    , tiPosition = (13, 33)
    , tiExpected =
      [ Completion (NameCompletion "id") (TypeCompletion "nat") (DocCompletion "")
      , Completion (NameCompletion "is_admin") (TypeCompletion "bool") (DocCompletion "")
      , CompletionKeyword (NameCompletion "if")
      , CompletionKeyword (NameCompletion "switch")
      ]
    }

  , TestInfo
    { tiContract = "type-constructor.ligo"
    , tiPosition = (5, 21)
    , tiExpected =
      [ Completion (NameCompletion "Increment") (TypeCompletion "action") (DocCompletion "")
      ]
    }
  , TestInfo
    { tiContract = "type-constructor.mligo"
    , tiPosition = (5, 19)
    , tiExpected =
      [ Completion (NameCompletion "Increment") (TypeCompletion "action") (DocCompletion "")
      ]
    }
  , TestInfo
    { tiContract = "type-constructor.religo"
    , tiPosition = (5, 19)
    , tiExpected =
      [ Completion (NameCompletion "Increment") (TypeCompletion "action") (DocCompletion "")
      ]
    }

  , TestInfo
    { tiContract = "unfinished-field-name.mligo"
    , tiPosition = (8, 27)
    , tiExpected =
      [ Completion (NameCompletion "sum") (TypeCompletion "int") (DocCompletion "")
      , CompletionKeyword (NameCompletion "struct")
      ]
    }

  , TestInfo
    { tiContract = "nested-fields.ligo"
    , tiPosition = (21, 37)
    , tiExpected =
      [ Completion (NameCompletion "series") (TypeCompletion "int") (DocCompletion "")
      , CompletionKeyword (NameCompletion "set")
      , CompletionKeyword (NameCompletion "list")
      , CompletionKeyword (NameCompletion "skip")
      , CompletionKeyword (NameCompletion "case")
      , CompletionKeyword (NameCompletion "else")
      , CompletionKeyword (NameCompletion "step")
      , CompletionKeyword (NameCompletion "is")
      , CompletionKeyword (NameCompletion "recursive")
      , CompletionKeyword (NameCompletion "const")
      , CompletionKeyword (NameCompletion "contains")
      ]
    }
  , TestInfo
    { tiContract = "nested-fields.mligo"
    , tiPosition = (18, 36)
    , tiExpected =
      [ Completion (NameCompletion "series") (TypeCompletion "int") (DocCompletion "")
      , CompletionKeyword (NameCompletion "struct")
      , CompletionKeyword (NameCompletion "lsl")
      , CompletionKeyword (NameCompletion "else")
      , CompletionKeyword (NameCompletion "lsr")
      ]
    }
  , TestInfo
    { tiContract = "nested-fields.religo"
    , tiPosition = (18, 36)
    , tiExpected =
      [ Completion (NameCompletion "series") (TypeCompletion "int") (DocCompletion "")
      , CompletionKeyword (NameCompletion "switch")
      , CompletionKeyword (NameCompletion "lsl")
      , CompletionKeyword (NameCompletion "else")
      , CompletionKeyword (NameCompletion "lsr")
      ]
    }
  , TestInfo
    { tiContract = "incr.mligo"
    , tiPosition = (3, 13)
    , tiExpected =
      [ Completion (NameCompletion "incr_my_stuff") (TypeCompletion "nat") (DocCompletion "")
      , CompletionKeyword (NameCompletion "begin")
      , CompletionKeyword (NameCompletion "in")
      ]
    }
  ]

completionDriver :: forall parser. HasScopeForest parser IO => [TestInfo] -> IO TestTree
completionDriver testInfos = do
  graph <- parseContractsWithDependenciesScopes @parser parsePreprocessed contractsDir
  pure $ testGroup "Completion" $ map (makeTestCase graph) testInfos
  where
    makeTestCase graph info =
      testCase (tiContract info) do
        let fp = contractsDir </> tiContract info
            position = uncurry point $ tiPosition info
            contract = fromJust $ lookupContract fp graph
            tree = contractTree contract
            results = complete position tree
        case (results, tiExpected info) of
          (Nothing, []) -> pure ()
          (Nothing, _) -> expectationFailure "Expected completion items, but got none"
          (Just [], []) -> pure ()
          (Just _, []) -> expectationFailure "Expected no completion items, but got them"
          (Just results', expected') -> results' `shouldMatchList` expected'
