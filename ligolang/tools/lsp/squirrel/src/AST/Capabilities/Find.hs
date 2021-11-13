module AST.Capabilities.Find
  ( CanSearch
  , TypeDefinitionRes (..)
  , findScopedDecl
  , findNodeAtPoint
  , rangeOf
  , definitionOf
  , typeDefinitionOf
  , typeDefinitionAt
  , dereferenceTspec
  , referencesOf
  ) where

import Control.Lens (_Just, (^.), (^?))
import Control.Monad
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Duplo.Lattice
import Duplo.Pretty
import Duplo.Tree

import AST.Scope (Level (..), lookupEnv, ofLevel)
import AST.Scope.ScopedDecl
  (Scope, ScopedDecl (..), Type (..), TypeDeclSpecifics (..), _TypeSpec,
   _ValueSpec, extractRefName, sdSpec, vdsTspec)
import AST.Skeleton (LIGO, SomeLIGO, nestedLIGO)

import Product
import Range

type CanSearch xs =
  ( Contains Scope xs
  , Contains Range xs
  , Contains (Maybe Level) xs
  , Contains [Text] xs
  , Modifies (Product xs)
  , Eq (Product xs)
  )

findScopedDecl
  :: CanSearch xs
  => Range
  -> SomeLIGO xs
  -> Maybe ScopedDecl
findScopedDecl pos tree = do
  node <- findNodeAtPoint pos tree
  let info = extract node
  let fullEnv = getElem info
  level <- getElem info
  let filtered = filter (ofLevel level) fullEnv
  lookupEnv (ppToText $ void node) filtered

findInfoAtPoint
  :: (Contains Range xs, Eq (Product xs))
  => Range -> SomeLIGO xs -> Maybe (Product xs)
findInfoAtPoint pos tree = extract <$> findNodeAtPoint pos tree

findNodeAtPoint
  :: (Contains Range xs, Eq (Product xs))
  => Range -> SomeLIGO xs -> Maybe (LIGO xs)
findNodeAtPoint pos tree =
  listToMaybe (spineTo (\i -> pos `leq` getRange i) (tree ^. nestedLIGO))

rangeOf
  :: CanSearch xs
  => Range
  -> SomeLIGO xs
  -> Maybe Range
rangeOf pos tree = do
  refs <- _sdRefs <$> findScopedDecl pos tree
  range <- getRange <$> findInfoAtPoint pos tree
  if range `elem` refs
    then Just range
    else Nothing

definitionOf
  :: CanSearch xs
  => Range
  -> SomeLIGO xs
  -> Maybe Range
definitionOf pos tree =
  _sdOrigin <$> findScopedDecl pos tree

-- | A type is declared if it has a name and a declaration corresponding to that
-- name in the scope. A type is inlined if it has a definition inlined into
-- something else (e.g. a variable definition). A type is not found if for some
-- reason it's not there (e.g. a variable lacks a type annotation).
data TypeDefinitionRes
  = TypeNotFound
  | TypeDeclared ScopedDecl
  | TypeInlined TypeDeclSpecifics

typeDefinitionOf :: CanSearch xs => Range -> SomeLIGO xs -> TypeDefinitionRes
typeDefinitionOf pos tree = fromMaybe TypeNotFound $ do
  scope <- getElem <$> findInfoAtPoint pos tree
  varDecl <- findScopedDecl pos tree
  tspec <- varDecl ^? sdSpec . _ValueSpec . vdsTspec . _Just
  Just $ case findTypeRefDeclaration scope (_tdsInit tspec) of
    Nothing -> TypeInlined tspec
    Just decl -> TypeDeclared decl

findTypeRefDeclaration :: Scope -> Type -> Maybe ScopedDecl
findTypeRefDeclaration scope typ = do
  refName <- extractRefName typ
  lookupEnv refName (filter (ofLevel TypeLevel) scope)

typeDefinitionAt :: CanSearch xs => Range -> SomeLIGO xs -> Maybe Range
typeDefinitionAt pos tree = case typeDefinitionOf pos tree of
  TypeNotFound -> Nothing
  TypeDeclared decl -> Just (_sdOrigin decl)
  TypeInlined tspec -> Just (_tdsInitRange tspec)

-- | If the type is an alias to a declared type, substitute the type with the
-- aliased type. Otherwise, leave it be.
--
-- If the aliased name is undeclared, leave the type be.
dereferenceTspec :: Scope -> TypeDeclSpecifics -> TypeDeclSpecifics
dereferenceTspec scope tspec = fromMaybe tspec $ do
  refDecl <- findTypeRefDeclaration scope (_tdsInit tspec)
  refDecl ^? sdSpec . _TypeSpec

referencesOf
  :: CanSearch xs
  => Range
  -> SomeLIGO xs
  -> Maybe [Range]
referencesOf pos tree =
  _sdRefs <$> findScopedDecl pos tree
