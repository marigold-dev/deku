module Test.Common.Util.Parsers
  ( checkFile
  ) where

import Control.Exception.Safe (try)
import Duplo (HandlerFailed (..))

import AST.Parser (parsePreprocessed)
import AST.Scope (pattern FindContract, HasScopeForest, ScopeError, addShallowScopes)
import Parser (collectTreeErrors)
import ParseTree (Source (Path))

import Test.Common.FixedExpectations (Expectation, HasCallStack, expectationFailure)

checkFile
  :: forall parser
   . (HasCallStack, HasScopeForest parser IO)
  => Bool
  -> FilePath
  -> Expectation
checkFile True (Path -> path) = do
  res <- try (parsePreprocessed path)
  case res of
    Left (err :: HandlerFailed) -> expectationFailure $
      "Parsing failed, but it shouldn't have. " <>
      "Error: " <> show err <> "."
    Right c@(FindContract _file tree msgs) -> case msgs' of
      _ : _ -> expectationFailure $
        "Parsing failed, but it shouldn't have. " <>
        "Messages: " <> show msgs' <> "."
      [] -> do
        res' <- try @_ @ScopeError (addShallowScopes @parser c)
        case res' of
          Left err -> expectationFailure $
            "Scoping failed, but it shouldn't have. " <>
            "Error: " <> show err <> "."
          Right (FindContract _file tree' msgs'') -> case msgs''' of
            _ : _ -> expectationFailure $
              "Scoping failed, but it shouldn't have. " <>
              "Messages: " <> show msgs''' <> "."
            [] -> pure ()
            where
              msgs''' = collectTreeErrors tree' <> msgs''
      where
        msgs' = collectTreeErrors tree <> msgs
checkFile False (Path -> path) = do
  res <- try @_ @HandlerFailed (parsePreprocessed path)
  case res of
    Right c@(FindContract _file tree []) -> case collectTreeErrors tree of
      [] -> expectationFailure "Parsing succeeded, but it shouldn't have."
      _ : _ -> do
        res' <- try @_ @ScopeError (addShallowScopes @parser c)
        case res' of
          Right (FindContract _file tree' []) -> case collectTreeErrors tree' of
            [] -> expectationFailure "Scoping succeeded, but it shouldn't have."
            _ : _ -> pure ()
          _ -> pure ()
    _ -> pure ()
