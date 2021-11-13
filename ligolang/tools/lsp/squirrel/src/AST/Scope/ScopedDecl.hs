module AST.Scope.ScopedDecl
  ( Scope
  , ScopedDecl (..)
  , sdName
  , sdOrigin
  , sdRefs
  , sdDoc
  , sdDialect
  , sdSpec
  , DeclarationSpecifics (..)
  , _TypeSpec
  , _ValueSpec
  , TypeDeclSpecifics (..)
  , tdsInitRange
  , tdsInit
  , Type (..)
  , _RecordType
  , TypeField (..)
  , tfName
  , tfTspec
  , TypeConstructor (..)
  , ValueDeclSpecifics (..)
  , vdsInitRange
  , vdsParams
  , vdsTspec
  , Parameter (..)

  , Constant (..)
  , Pattern (..)
  , RecordFieldPattern (..)

  , IsLIGO (..)

  , Accessor
  , accessField
  , lppDeclCategory
  , lppLigoLike
  , fillTypeIntoCon
  , extractRefName
  ) where

import Control.Applicative ((<|>))
import Control.Lens ((%~), (&), (^?))
import Control.Lens.TH (makeLenses, makePrisms)
import Data.List (find)
import Data.Sum (inject)
import Data.Text (Text)
import Duplo.Tree (Cofree ((:<)), Element)

import AST.Pretty (Doc, Pretty (pp), lppDialect, sexpr)
import AST.Skeleton (LIGO, Lang, RawLigoList)
import AST.Skeleton qualified as LIGO
import Parser (fillInfo)
import Product (Product (Nil))
import Range (Range)
import Util (safeIndex)

type Scope = [ScopedDecl]

data ScopedDecl = ScopedDecl
  { _sdName :: Text
  , _sdOrigin :: Range
  , _sdRefs :: [Range]
  , _sdDoc :: [Text]
  , _sdDialect :: Lang
  , _sdSpec :: DeclarationSpecifics
  }

data DeclarationSpecifics
  = TypeSpec TypeDeclSpecifics
  | ValueSpec ValueDeclSpecifics

data TypeDeclSpecifics = TypeDeclSpecifics
  { _tdsInitRange :: Range
  , _tdsInit :: Type
  }
  deriving stock (Eq, Show)

data Type
  = RecordType [TypeField]
  | VariantType [TypeConstructor]
  | TupleType [TypeDeclSpecifics]
  | ApplyType Type [Type]
  | AliasType Text
  | ArrowType Type Type
  deriving stock (Eq, Show)

data TypeField = TypeField
  { _tfName :: Text
  , _tfTspec :: TypeDeclSpecifics
  }
  deriving stock (Eq, Show)

newtype TypeConstructor = TypeConstructor
  { _tcName :: Text
  }
  deriving stock (Eq, Show)

data ValueDeclSpecifics = ValueDeclSpecifics
  { _vdsInitRange :: Maybe Range
  , _vdsParams :: Maybe [Parameter] -- if there are any, it's a function
  , _vdsTspec :: Maybe TypeDeclSpecifics
  }
  deriving stock (Eq, Show)

data Parameter
  = ParameterPattern Pattern
  | ParameterBinding Pattern (Maybe Type)
  deriving stock (Eq, Show)

data Constant
  = Int    Text
  | Nat    Text
  | String Text
  | Float  Text
  | Bytes  Text
  | Tez    Text
  deriving stock (Eq, Show)

data Pattern
  = IsConstr     Text (Maybe Pattern)
  | IsConstant   Constant
  | IsVar        Text
  | IsCons       Pattern Pattern
  | IsAnnot      Pattern Type  -- Semantically `Var`
  | IsWildcard
  | IsSpread     Pattern
  | IsList       [Pattern]
  | IsTuple      [Pattern]
  | IsRecord     [RecordFieldPattern]
  | IsParen      Pattern
  deriving stock (Eq, Show)

data RecordFieldPattern
  = IsRecordField Text Pattern
  | IsRecordCapture Text
  deriving stock (Eq, Show)

instance Eq ScopedDecl where
  sd1 == sd2 =
    _sdName sd1 == _sdName sd2 &&
    _sdOrigin sd1 == _sdOrigin sd2

instance Ord ScopedDecl where
  sd1 `compare` sd2 =
    _sdName sd1 `compare` _sdName sd2 <>
    _sdOrigin sd1 `compare` _sdOrigin sd2

instance Pretty ScopedDecl where
  pp (ScopedDecl n o refs doc _ _) =
    sexpr "decl" [pp n, pp o, pp refs, pp doc]

lppDeclCategory :: ScopedDecl -> Doc
lppDeclCategory decl = case _sdSpec decl of
  TypeSpec tspec -> lppLigoLike (_sdDialect decl) tspec
  ValueSpec vspec -> case _vdsTspec vspec of
    Nothing -> pp @Text "unknown"
    Just tspec -> lppLigoLike (_sdDialect decl) tspec

lppLigoLike :: IsLIGO a => Lang -> a -> Doc
lppLigoLike dialect ligoLike = lppDialect dialect (fillInfo (toLIGO ligoLike))

class IsLIGO a where
  toLIGO :: a -> LIGO '[]

instance IsLIGO TypeDeclSpecifics where
  toLIGO tspec = toLIGO (_tdsInit tspec)

instance IsLIGO Type where
  toLIGO (RecordType fields) = node (LIGO.TRecord (map toLIGO fields))
  toLIGO (VariantType cons) = node (LIGO.TSum (map toLIGO cons))
  toLIGO (TupleType typs) = node (LIGO.TProduct (map toLIGO typs))
  toLIGO (AliasType typ) = node (LIGO.TypeName typ)
  toLIGO (ApplyType name types) = node (LIGO.TApply (toLIGO name) (map toLIGO types))
  toLIGO (ArrowType left right) = node (LIGO.TArrow (toLIGO left) (toLIGO right))

instance IsLIGO TypeField where
  toLIGO TypeField{ .. } = node
    (LIGO.TField (node (LIGO.FieldName _tfName)) (toLIGO _tfTspec))

instance IsLIGO TypeConstructor where
  toLIGO TypeConstructor{ .. } = node
    (LIGO.Variant (node (LIGO.Ctor _tcName)) Nothing)

instance IsLIGO Parameter where
  toLIGO (ParameterPattern pat) = toLIGO pat
  toLIGO (ParameterBinding pat typM) = node (LIGO.BParameter (toLIGO pat) (toLIGO <$> typM))

instance IsLIGO Constant where
  toLIGO (Int i) = node (LIGO.Int i)
  toLIGO (Nat n) = node (LIGO.Nat n)
  toLIGO (String s) = node (LIGO.String s)
  toLIGO (Float f) = node (LIGO.Float f)
  toLIGO (Bytes b) = node (LIGO.Bytes b)
  toLIGO (Tez t) = node (LIGO.Tez t)

instance IsLIGO Pattern where
  toLIGO (IsConstr name patM) = node (LIGO.IsConstr (node (LIGO.Ctor name)) (toLIGO <$> patM))
  toLIGO (IsConstant constant) = node (LIGO.IsConstant (toLIGO constant))
  toLIGO (IsVar name) = node (LIGO.IsVar (node (LIGO.NameDecl name)))
  toLIGO (IsCons left right) = node (LIGO.IsCons (toLIGO left) (toLIGO right))
  toLIGO (IsAnnot pat typ) = node (LIGO.IsAnnot (toLIGO pat) (toLIGO typ))
  toLIGO IsWildcard = node LIGO.IsWildcard
  toLIGO (IsSpread pat) = node (LIGO.IsSpread (toLIGO pat))
  toLIGO (IsList pats) = node (LIGO.IsList (toLIGO <$> pats))
  toLIGO (IsTuple pats) = node (LIGO.IsTuple (toLIGO <$> pats))
  toLIGO (IsRecord pats) = node (LIGO.IsRecord (toLIGO <$> pats))
  toLIGO (IsParen pat) = node (LIGO.IsParen (toLIGO pat))

instance IsLIGO RecordFieldPattern where
  toLIGO (IsRecordField name body) = node (LIGO.IsRecordField (node (LIGO.FieldName name)) (toLIGO body))
  toLIGO (IsRecordCapture name) = node (LIGO.IsRecordCapture (node (LIGO.NameDecl name)))

node :: Element f RawLigoList => f (LIGO '[]) -> LIGO '[]
node element = Nil :< inject element

$(makeLenses ''ScopedDecl)
$(makePrisms ''DeclarationSpecifics)
$(makeLenses ''TypeDeclSpecifics)
$(makeLenses ''ValueDeclSpecifics)
$(makePrisms ''Type)
$(makeLenses ''TypeField)

-- | Assuming that 'typDecl' is a declaration of a type containing a constructor
-- that has a declaration 'conDecl', specify 'conDecl''s type as that of
-- 'typDecl'.
fillTypeIntoCon :: ScopedDecl -> ScopedDecl -> ScopedDecl
fillTypeIntoCon typDecl conDecl
  = conDecl & sdSpec . _ValueSpec . vdsTspec %~ (<|> Just tspec)
  where
    typ = AliasType (_sdName typDecl)
    tspec = TypeDeclSpecifics
      { _tdsInitRange = _sdOrigin typDecl
      , _tdsInit = typ
      }

-- | If the type is just a reference to another type, extract a name of that
-- reference.
extractRefName :: Type -> Maybe Text
extractRefName typ = typ ^? _AliasType

type Accessor = Either Int Text

accessField :: TypeDeclSpecifics -> Accessor -> Maybe TypeDeclSpecifics
accessField tspec (Left num) = do
  tupleTspecs <- tspec ^? tdsInit . _TupleType
  safeIndex tupleTspecs num
accessField tspec (Right text) = do
  typeFields <- tspec ^? tdsInit . _RecordType
  fitting <- find ((text ==) . _tfName) typeFields
  pure (_tfTspec fitting)
