module AST.CodeAction
  ( collectCodeActions
  ) where

import Control.Exception.Safe (MonadCatch)

import Language.LSP.Types qualified as J

import AST.Scope
import AST.Skeleton
import Range

import AST.Capabilities.CodeAction.ExtractTypeAlias as Exports
import Control.Monad

collectCodeActions
  :: MonadCatch m
  => Range
  -> J.CodeActionContext
  -> J.Uri
  -> SomeLIGO Info'
  -> m [J.CodeAction]
collectCodeActions at _con uri tree = join <$> sequence
  [ typeExtractionCodeAction at uri tree
  ]
