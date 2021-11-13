-- | Code folding utilities

module AST.Capabilities.Folding
  ( foldingAST
  , toFoldingRange
  ) where

import Language.LSP.Types qualified as J

import Duplo.Tree

import AST.Scope
import AST.Skeleton
import Control.Monad.Catch.Pure (MonadCatch)

import Control.Monad.Writer.Strict
import Product
import Range

-- | Fold the given ast by visiting its nodes and applying provided
-- handler to them. This is used primarly to call `Core.sendFunc`
-- at each specific node we face.
-- TODO: may affect perfomance, if so we need to use `Endo` instead.
foldingAST
  :: (MonadCatch m)
  => LIGO Info'
  -> m [Range]
foldingAST = execWriterT . visit handlers
  where
    handlers =
      [ Visit @Binding $ \case
          (getElem @Range -> r, BFunction {}) -> tell [r]
          (getElem @Range -> r, BTypeDecl {}) -> tell [r]
          -- TODO: include blocks?
          _ -> pure ()
      , Visit @Expr $ \case
          (getElem @Range -> r, If {}) -> tell [r]
          (getElem @Range -> r, Case {}) -> tell [r]
          (getElem @Range -> r, Seq {}) -> tell [r]
          (getElem @Range -> r, Lambda {}) -> tell [r]
          (getElem @Range -> r, ForLoop {}) -> tell [r]
          (getElem @Range -> r, ForBox {}) -> tell [r]
          _ -> pure ()
      ]

toFoldingRange :: Range -> J.FoldingRange
toFoldingRange Range
  { _rStart  = (_startLine, Just -> _startCharacter, _)
  , _rFinish = (_endLine, Just -> _endCharacter, _)
  } = J.FoldingRange
  { _startLine = _startLine - 1
  , _startCharacter = pred <$> _startCharacter
  , _endLine = _endLine - 1
  , _endCharacter = pred <$> _endCharacter
  , _kind = Just J.FoldingRangeRegion
  }
