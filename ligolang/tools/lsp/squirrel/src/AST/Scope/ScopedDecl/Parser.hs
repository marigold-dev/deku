module AST.Scope.ScopedDecl.Parser
  ( parseType
  , parseTypeDeclSpecifics
  , parseParameters
  ) where

import Control.Lens ((??))
import Data.Foldable (asum)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe, mapMaybe)
import Duplo.Tree (layer)

import AST.Pretty (PPableLIGO, ppToText)
import AST.Scope.ScopedDecl
import AST.Skeleton (LIGO)
import AST.Skeleton qualified as LIGO
import Range (getRange)

-- * Parsers for type.

parseTypeDeclSpecifics :: PPableLIGO info => LIGO info -> TypeDeclSpecifics
parseTypeDeclSpecifics node = TypeDeclSpecifics
  { _tdsInitRange = getRange node
  , _tdsInit = parseType node
  }

-- The node is _always_ parsed as some type. In the worst case — if the node is
-- not a type, it's parsed as an alias type with the node textual representation
-- as its content.
--
-- Also see 'parseAliasType'.
parseType :: PPableLIGO info => LIGO info -> Type
parseType node =
  fromMaybe (parseAliasType node) (asum (parsers ?? node))
  where
    parsers =
      [ parseRecordType
      , parseVariantType
      , parseTupleType
      , parseApplyType
      , parseArrowType
      ]

parseArrowType :: PPableLIGO info => LIGO info -> Maybe Type
parseArrowType node = do
  LIGO.TArrow left right <- layer node
  let left' = parseType left
  let right' = parseType right
  pure $ ArrowType left' right'

parseApplyType :: PPableLIGO info => LIGO info -> Maybe Type
parseApplyType node = do
  LIGO.TApply name types <- layer node
  let name' = parseType name
  let types' = map parseType types
  pure $ ApplyType name' types'

parseRecordType :: PPableLIGO info => LIGO info -> Maybe Type
parseRecordType node = do
  LIGO.TRecord fieldNodes <- layer node
  let typeFields = mapMaybe parseTypeField fieldNodes
  pure (RecordType typeFields)

parseTypeField :: PPableLIGO info => LIGO info -> Maybe TypeField
parseTypeField node = do
  LIGO.TField nameNode typNode <- layer node
  LIGO.FieldName _tfName <- layer nameNode
  let _tfTspec = parseTypeDeclSpecifics typNode
  pure TypeField{ .. }

parseVariantType :: LIGO info -> Maybe Type
parseVariantType node = do
  LIGO.TSum conNodes <- layer node
  let cons = mapMaybe parseTypeConstructor conNodes
  pure (VariantType cons)

parseTypeConstructor :: LIGO info -> Maybe TypeConstructor
parseTypeConstructor node = do
  LIGO.Variant conNameNode _ <- layer node
  LIGO.Ctor _tcName <- layer conNameNode
  pure TypeConstructor{ .. }

parseTupleType :: PPableLIGO info => LIGO info -> Maybe Type
parseTupleType node = do
  LIGO.TProduct elementNodes <- layer node
  let elements = map parseTypeDeclSpecifics elementNodes
  pure (TupleType elements)

-- Since we don't care right now about distinguishing functions or whatever, we
-- just treat the whole node as a type name. It _is_ possible that the node is
-- not even a type: it could be an error node. However we choose to fail, we'll
-- lose the whole type structure instead of this one leaf.
parseAliasType :: PPableLIGO info => LIGO info -> Type
parseAliasType node = AliasType (ppToText node)

-- * Parsers for parameters.

parseParameter :: PPableLIGO info => LIGO info -> Maybe Parameter
parseParameter node = asum (parsers ?? node)
  where
    parsers =
      [ fmap ParameterPattern . parsePattern
      , parseBParameter
      ]

parseBParameter :: PPableLIGO info => LIGO info -> Maybe Parameter
parseBParameter node = do
  LIGO.BParameter name typ <- layer node
  ParameterBinding <$> parsePattern name <*> pure (parseType <$> typ)

parseParameters :: PPableLIGO info => [LIGO info] -> [Parameter]
parseParameters = mapMaybe parseParameter

-- * Parsers for patterns.

parseConstant :: LIGO info -> Maybe Constant
parseConstant node = layer node <&> \case
  LIGO.Int    i -> Int    i
  LIGO.Nat    n -> Nat    n
  LIGO.String s -> String s
  LIGO.Float  f -> Float  f
  LIGO.Bytes  b -> Bytes  b
  LIGO.Tez    t -> Tez    t

parsePattern :: PPableLIGO info => LIGO info -> Maybe Pattern
parsePattern node = layer node >>= \case
  LIGO.IsConstr name patM -> pure $ IsConstr (ppToText name) (parsePattern =<< patM)
  LIGO.IsConstant constant -> IsConstant <$> parseConstant constant
  LIGO.IsVar name -> pure $ IsVar $ ppToText name
  LIGO.IsCons left right -> IsCons <$> parsePattern left <*> parsePattern right
  LIGO.IsAnnot pat typ -> IsAnnot <$> parsePattern pat <*> pure (parseType typ)
  LIGO.IsWildcard -> pure IsWildcard
  LIGO.IsSpread pat -> IsSpread <$> parsePattern pat
  LIGO.IsList pats -> pure $ IsList $ mapMaybe parsePattern pats
  LIGO.IsTuple pats -> pure $ IsTuple $ mapMaybe parsePattern pats
  LIGO.IsRecord pats -> pure $ IsRecord $ mapMaybe parseRecordFieldPattern pats
  LIGO.IsParen pat -> IsParen <$> parsePattern pat

parseRecordFieldPattern :: PPableLIGO info => LIGO info -> Maybe RecordFieldPattern
parseRecordFieldPattern node = layer node >>= \case
  LIGO.IsRecordField name body -> IsRecordField (ppToText name) <$> parsePattern body
  LIGO.IsRecordCapture name -> pure $ IsRecordCapture (ppToText name)
