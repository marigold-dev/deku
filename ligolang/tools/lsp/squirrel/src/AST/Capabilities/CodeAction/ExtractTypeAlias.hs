-- Extract type alias capability.
-- This capability allows for user to extract any type from a contract by
-- making a type alias for it and replacing all the occurences of the aliased
-- type with its alias.
--
-- For instance, having:

-- ```
-- #include "a.ligo"
-- #include "b.ligo"
--
-- function id (const i : int) : int * int is (i, i)
--
-- function f (const r : int * int) : int is 2
-- ```
--
-- with cursor pointing at `*` in `int * int` and selecting `extract type alias`
-- refactoring method this produces a `CodeAction` that replaces parts of the
-- contract tree to
--
-- ```
-- #include "a.ligo"
-- #include "b.ligo"
-- type x is int * int
--
-- function id (const i : int) : x is (i, i)
--
-- function f (const r : x) : int is 2
-- ```
--
-- note that type alias is added *after* the block with includes. I (awkure) decided
-- that's its more convenient this way.

module AST.Capabilities.CodeAction.ExtractTypeAlias
  ( typeExtractionCodeAction
  , extractedTypeNameAlias
  ) where

import Control.Exception.Safe (MonadCatch)

import Control.Monad.Trans.Writer
import Data.Text qualified as T

import Data.HashMap.Strict qualified as HM
import Language.LSP.Types qualified as J

import AST.Pretty
import AST.Scope
import AST.Skeleton
import Duplo
import Parser
import Product
import Range

extractedTypeNameAlias :: String
extractedTypeNameAlias = "extractedTypeName"

-- | Construct all code actions regarding type extraction for the contract.
typeExtractionCodeAction
  :: MonadCatch m
  => Range
  -> J.Uri
  -> SomeLIGO Info'
  -> m [J.CodeAction]
typeExtractionCodeAction at uri (SomeLIGO dialect tree) =
  let
    isPreprocessor = \case
      (match -> Just (_, Preprocessor _)) -> True
      _ -> False
  in case match tree of
    (Just (i, RawContract xs)) -> do
      let
        filteredSubtree = dropWhile isPreprocessor xs
        filteredSubtreeRange = case filteredSubtree of
          [] -> getElem @Range i
          (x:_) -> getElem @Range $ extract x
        strippedTree =  make (i, RawContract filteredSubtree)
      case spineTo (leq at . getElem) strippedTree of
        ((match -> Just (info, TypeName tn)):_) -> do
          typeAliasName <- genTypeName tree
          typeEdits <- makeReplaceTypeEdits typeAliasName (Right tn) strippedTree
          let
            replaceRange = getElem @Range info
            typeAlias = constructTypeAlias dialect typeAliasName (Left tn) filteredSubtreeRange
          return $ mkCodeAction uri replaceRange (typeEdits <> [typeAlias])
        (typeNode@(match @Type -> Just (info, exactType)):_) -> do
          typeAliasName <- genTypeName tree
          typeEdits <- makeReplaceTypeEdits typeAliasName (Left exactType) strippedTree
          let
            replaceRange = getElem @Range info
            typeAlias = constructTypeAlias dialect typeAliasName (Right typeNode) filteredSubtreeRange
          return $ mkCodeAction uri replaceRange (typeEdits <> [typeAlias])
        _ -> return [] -- Matched everything but type, ignore
    _ -> return [] -- Malformed tree with error nodes passed, ignore

-- | Generate fresh type alias that is not found
-- in the given tree.
-- TODO: Somewhat the tree, passed to `typeExtractionCodeAction`
-- lacks of `ScopedDecl`s which is strange since we parse trees
-- in our ASTMap with scopes as well. So I (awkure) decided
-- that we return blank `extractedTypeNameAlias` instead for now
-- since user may want to rename the type anyway.
genTypeName :: Monad m => LIGO Info' -> m T.Text
genTypeName _tree =
  -- let
  --   decls = getElem @[SD.ScopedDecl] $ extract tree
  --   findInTree proposedName = flip Foldable.find decls \case
  --     SD.ScopedDecl{_sdName = givenName, _sdSpec = SD.TypeSpec _}
  --       | proposedName == givenName -> True
  --     _ -> False
  --   name
  --     = head
  --     . filterOutFirst (isJust . findInTree)
  --     $ ("t"<>) . T.pack . show @Integer <$> [0..]
  return $ T.pack extractedTypeNameAlias

-- | Reconstructs type definition node from given alias name and
-- either if it's a typename or some other complex type.
constructTypeAlias
  :: Lang
  -> T.Text -- Given type alias
  -> Either T.Text (LIGO Info') -- Either type name or type node
  -> Range -- range of the topmost level of the stripped tree
  -> J.TextEdit
constructTypeAlias dialect alias t Range{_rStart = (sl, sc, _)} =
  J.TextEdit
    { _range = toLspRange $ point sl sc
    , _newText = T.pack . (<>"\n") . show . lppDialect @(LIGO Info') dialect $
        case t of
          (Left typeName) ->
            defaultState :< inject @Binding
              (BTypeDecl
                (defaultState :< inject @TypeName (TypeName alias))
                (defaultState :< inject @TypeName (TypeName typeName)))
          (Right typeNode) ->
            defaultState :< inject @Binding
              (BTypeDecl
                (defaultState :< inject @TypeName (TypeName alias))
                typeNode)
    }

defaultState :: Product Info'
defaultState = [] :> Nothing :> PreprocessedRange (point 1 1) :> [] :> [] :> point 1 1 :> N :> CodeSource "" :> Nil

-- | Diagnostics collected for every type that allows for
-- type extraction code action to be clicked.
mkDiagnostics :: Range -> J.Diagnostic
mkDiagnostics r = J.Diagnostic
  { _range = toLspRange r
  , _severity = Just J.DsHint
  , _code = Nothing
  , _source = Nothing
  , _message = "extract type alias"
  , _tags = Nothing
  , _relatedInformation = Nothing
  }

-- | Constructs code action for type extraction.
mkCodeAction :: J.Uri -> Range -> [J.TextEdit] -> [J.CodeAction]
mkCodeAction uri replaceRange typeEdits =
  [ J.CodeAction
    { _title = "extract type definition"
    , _kind = Just J.CodeActionRefactorExtract
    , _diagnostics = Just $ J.List [mkDiagnostics replaceRange]
    , _isPreferred = Just False
    , _disabled = Nothing
    , _edit = Just J.WorkspaceEdit
        {
          _changes = Just $ HM.fromList [(uri, J.List typeEdits)]
          -- TODO: It's implied to use `_documentChanges` instead of `_changes` in case
          -- we are changing only one file, but, as it happens, vscode editor rejects
          -- these changes without any proper explanation why. Meaning that even though
          -- client received response of this code action, the text itself remains unchanged.
        , _documentChanges = Nothing
        , _changeAnnotations = Nothing
        }
    , _command = Nothing
    , _xdata = Nothing
    }
  ]

-- | Construct edits for replacing existing type that matches either
-- the given constructed type or type alias.
makeReplaceTypeEdits
  :: (MonadCatch m)
  => T.Text -- New type name to be replaced with
  -> Either (Type (LIGO Info')) T.Text -- Either it's a constructed type or a type alias
  -> LIGO Info'
  -> m [J.TextEdit]
makeReplaceTypeEdits newTypeName (Left typeNode) originTree =
  execWriterT $ visit
    [ Visit @Type $ \case
        (getElem @Range -> r, typeNode') | typeNode == typeNode' ->
          tell [J.TextEdit { _range = toLspRange r, _newText = newTypeName }]
        _ -> pure ()
    ]
    originTree
makeReplaceTypeEdits newTypeName (Right oldTypeName) originTree =
  execWriterT $ visit
    [ Visit @TypeName $ \case
        (getElem @Range -> r, TypeName typeName') | oldTypeName == typeName' ->
          tell [J.TextEdit { _range = toLspRange r, _newText = newTypeName }]
        _ -> pure ()
    ]
    originTree
