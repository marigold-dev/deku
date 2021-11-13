{-# LANGUAGE RecordWildCards #-}

module AST.Scope.FromCompiler
  ( FromCompiler
  ) where

import Control.Category ((>>>))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Lens ((%~))
import Data.Function (on)
import Data.HashMap.Strict ((!))
import Data.Map (Map)
import Data.Map qualified as Map
import Duplo.Lattice
import Duplo.Tree (make, only)

import AST.Scope.Common
import AST.Scope.ScopedDecl (DeclarationSpecifics (..), ScopedDecl (..), ValueDeclSpecifics (..))
import AST.Scope.ScopedDecl.Parser (parseTypeDeclSpecifics)
import AST.Skeleton (Lang, SomeLIGO (..))
import Cli
import ListZipper (atLocus, find, withListZipper)
import Product
import Range
import Util (removeDots)
import Util.Graph (traverseAMConcurrently)

data FromCompiler

-- FIXME: If one contract throws an exception, the entire thing will fail. Standard
-- scopes will use Fallback.
instance (HasLigoClient m, MonadUnliftIO m) => HasScopeForest FromCompiler m where
  scopeForest = traverseAMConcurrently \(FindContract ast (SomeLIGO dialect _) msg) -> do
    (defs, _) <- getLigoDefinitions ast
    pure $ FindContract ast (fromCompiler dialect defs) msg

-- | Extract `ScopeForest` from LIGO scope dump.
fromCompiler :: Lang -> LigoDefinitions -> ScopeForest
fromCompiler dialect (LigoDefinitions decls scopes) =
    foldr (buildTree decls) (ScopeForest [] Map.empty) scopes
  where
    -- For a new scope to be injected, grab its range and decl and start
    -- injection process.
    buildTree :: LigoDefinitionsInner -> LigoScope -> ScopeForest -> ScopeForest
    buildTree (LigoDefinitionsInner decls') (LigoScope r es _) = do
      let ds = Map.fromList $ map (fromLigoDecl . (decls' !)) es
      let rs = Map.keysSet ds
      let r' = normalizeRange $ fromLigoRangeOrDef r
      injectScope (make (rs :> r' :> Nil, []), ds)

    normalizeRange :: Range -> Range
    normalizeRange = rFile %~ removeDots

    -- LIGO compiler provides no comments, so they left [].
    fromLigoDecl :: LigoDefinitionScope -> (DeclRef, ScopedDecl)
    fromLigoDecl (LigoDefinitionScope n orig bodyR ty refs) = do
      let r = normalizeRange $ fromLigoRangeOrDef orig
      let rs = normalizeRange . fromLigoRangeOrDef <$> refs
      ( DeclRef n r
       , ScopedDecl n r (r : rs) [] dialect (ValueSpec vspec)
       )
      where
        _vdsInitRange = mbFromLigoRange bodyR
        _vdsParams = Nothing
        _vdsTspec = parseTypeDeclSpecifics . fromLigoTypeFull <$> ty
        vspec = ValueDeclSpecifics{ .. }

    -- Find a place for a scope inside a ScopeForest.
    injectScope :: (ScopeTree, Map DeclRef ScopedDecl) -> ScopeForest -> ScopeForest
    injectScope (subject, ds') (ScopeForest forest ds) =
        ScopeForest (loop forest) (ds <> ds')
      where
        loop
          = withListZipper
          $ find (subject `isCoveredBy`) >>> atLocus maybeLoop

        isCoveredBy = leq `on` getRange

        -- If there are no trees above subject here, just put it in.
        -- Otherwise, put it in a tree that covers it.
        maybeLoop :: Maybe ScopeTree -> Maybe ScopeTree
        maybeLoop = Just . maybe subject restart

        -- Take a forest out of tree, loop, put it back.
        restart (only -> (r, trees)) = make (r, loop trees)
