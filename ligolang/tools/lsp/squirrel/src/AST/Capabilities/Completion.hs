module AST.Capabilities.Completion
  ( Completion (..)
  , NameCompletion (..)
  , TypeCompletion (..)
  , DocCompletion (..)
  , completionName
  , complete
  , toCompletionItem
  ) where

import Language.LSP.Types (CompletionDoc (..), CompletionItem (..), CompletionItemKind (..))

import Control.Lens ((^?))
import Control.Monad (foldM)
import Data.Foldable (asum)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List (isSubsequenceOf, nubBy)
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet (filter, toList)
import Data.Text (Text)
import Data.Text qualified as Text
import Duplo.Lattice
import Duplo.Pretty
import Duplo.Tree

import AST.Capabilities.Find
  (TypeDefinitionRes (..), dereferenceTspec, findNodeAtPoint, typeDefinitionOf)
import AST.Pretty (PPableLIGO, docToText)
import AST.Scope
import AST.Scope.ScopedDecl
  (Accessor, DeclarationSpecifics (..), Scope, ScopedDecl (..), TypeDeclSpecifics (..),
  TypeField (..), _RecordType, _TypeSpec, accessField, lppDeclCategory, lppLigoLike, sdSpec,
  tdsInit)
import AST.Skeleton
import Product
import Range
import Util (unconsFromEnd)

newtype NameCompletion = NameCompletion { getNameCompletion :: Text } deriving newtype (Eq, Show)
newtype TypeCompletion = TypeCompletion { getTypeCompletion :: Text } deriving newtype (Eq, Show)
newtype DocCompletion  = DocCompletion  { getDocCompletion  :: Text } deriving newtype (Eq, Show)

data Completion
  = Completion NameCompletion TypeCompletion DocCompletion
  | CompletionKeyword NameCompletion
  deriving stock (Eq, Show)

completionName :: Completion -> NameCompletion
completionName (Completion name _ _) = name
completionName (CompletionKeyword name) = name

type CompletionLIGO info =
  ( Eq (Product info)
  , Modifies (Product info)
  , Contains Range info
  , Contains Scope info
  , Contains (Maybe Level) info
  , PPableLIGO info
  )

complete :: CompletionLIGO xs => Range -> SomeLIGO xs -> Maybe [Completion]
complete pos tree = do
  node <- findNodeAtPoint pos tree
  let scope = getElem (extract node)
  let nameLevel = getElem (extract node)
  getPossibleCompletions scope nameLevel pos tree
    <&> nubBy ((==) `on` getNameCompletion . completionName)
    <&> filter (isSubseqOf (ppToText node) . getNameCompletion . completionName)

getPossibleCompletions
  :: CompletionLIGO xs
  => Scope -> Maybe Level -> Range -> SomeLIGO xs -> Maybe [Completion]
getPossibleCompletions scope level pos tree = mconcat
  [ asum completers
  , completeKeyword pos tree
  ]
  where
    completers =
      [ completeField scope pos tree
      , completeFromScope scope level
      ]

parseAccessor :: PPableLIGO xs => LIGO xs -> Accessor
parseAccessor node = case reads (Text.unpack textValue) of
  [(num, "")] -> Left num
  _ -> Right textValue
  where
    textValue = ppToText node

completeKeyword :: CompletionLIGO xs => Range -> SomeLIGO xs -> Maybe [Completion]
completeKeyword pos tree@(SomeLIGO dialect _) = do
  cover <- findNodeAtPoint pos tree
  let completions = HashSet.filter (ppToText cover `isSubseqOf`) keywords
  case HashSet.toList completions of
    [] -> Nothing
    completions' -> Just $ CompletionKeyword . NameCompletion <$> completions'
  where
    keywords :: HashSet Text
    keywords = case dialect of
      Caml   -> cameLIGOKeywords
      Pascal -> pascaLIGOKeywords
      Reason -> reasonLIGOKeywords

completeField
  :: CompletionLIGO xs => Scope -> Range -> SomeLIGO xs -> Maybe [Completion]
completeField = completeFieldTypeAware

completeFieldTypeAware
  :: CompletionLIGO xs => Scope -> Range -> SomeLIGO xs -> Maybe [Completion]
completeFieldTypeAware scope pos tree@(SomeLIGO dialect nested) = do
  QualifiedName{ qnSource, qnPath } <- asum (map layer covers)
  -- throwing away the last field, because it's the field we are trying to complete
  (finished, _unfinished) <- unconsFromEnd qnPath
  let accessors = map parseAccessor finished
  firstTspec <- toTspec (typeDefinitionOf (getRange qnSource) tree)
  finalTspec <- foldM accessAndDereference firstTspec accessors
  finalFields <- finalTspec ^? tdsInit . _RecordType
  pure (map mkCompletion finalFields)
  where
    covers = spineTo (leq pos . getRange) nested

    toTspec TypeNotFound = Nothing
    toTspec (TypeDeclared decl) = decl ^? sdSpec . _TypeSpec
    toTspec (TypeInlined tspec) = Just tspec

    accessAndDereference :: TypeDeclSpecifics -> Accessor -> Maybe TypeDeclSpecifics
    accessAndDereference tspec accessor =
      dereferenceTspec scope <$> accessField tspec accessor

    mkCompletion field = Completion
      (NameCompletion $ _tfName field)
      (TypeCompletion $ docToText (lppLigoLike dialect (_tfTspec field)))
      (DocCompletion "")

completeFromScope :: Scope -> Maybe Level -> Maybe [Completion]
completeFromScope scope level
  = Just [asCompletion decl | decl <- scope, decl `fitsLevel` level]

toCompletionItem :: Completion -> CompletionItem
toCompletionItem c@(Completion (NameCompletion cName) (TypeCompletion cType) _) =
  CompletionItem
  { _label = cName
  , _kind = Just CiFunction -- TODO (LIGO-226)
  , _detail = Just $ ": " <> cType -- TODO (LIGO-227): more elaborate info
  , _documentation = mkDoc c
  , _deprecated = Nothing
  , _preselect = Nothing
  , _sortText = Nothing
  , _filterText = Nothing
  , _insertTextFormat = Nothing
  , _textEdit = Nothing
  , _insertText = Nothing
  , _additionalTextEdits = Nothing
  , _commitCharacters = Nothing
  , _command = Nothing
  , _xdata = Nothing
  , _tags = Nothing
  , _insertTextMode = Nothing
  }
toCompletionItem (CompletionKeyword (NameCompletion cName)) =
  CompletionItem
  { _label = cName
  , _kind = Just CiKeyword
  , _detail = Nothing
  , _documentation = Nothing
  , _deprecated = Nothing
  , _preselect = Nothing
  , _sortText = Nothing
  , _filterText = Nothing
  , _insertTextFormat = Nothing
  , _textEdit = Nothing
  , _insertText = Nothing
  , _additionalTextEdits = Nothing
  , _commitCharacters = Nothing
  , _command = Nothing
  , _xdata = Nothing
  , _tags = Nothing
  , _insertTextMode = Nothing
  }

mkDoc :: Completion -> Maybe CompletionDoc
mkDoc (CompletionKeyword (NameCompletion _cName)) = Nothing
mkDoc (Completion (NameCompletion cName) (TypeCompletion cType) (DocCompletion cDoc)) =
  Just . CompletionDocString $
    cName <> " is of type " <> cType <> ". " <> cDoc

asCompletion :: ScopedDecl -> Completion
asCompletion sd = Completion
  (NameCompletion $ ppToText (_sdName sd))
  (TypeCompletion $ docToText (lppDeclCategory sd))
  (DocCompletion  $ ppToText (fsep $ map pp $ _sdDoc sd))

isSubseqOf :: Text -> Text -> Bool
isSubseqOf l r = isSubsequenceOf (Text.unpack l) (Text.unpack r)

fitsLevel :: ScopedDecl -> Maybe Level -> Bool
fitsLevel _ Nothing = True
fitsLevel decl (Just level) = case (_sdSpec decl, level) of
  (ValueSpec{}, TermLevel) -> True
  (TypeSpec{}, TypeLevel) -> True
  _ -> False
