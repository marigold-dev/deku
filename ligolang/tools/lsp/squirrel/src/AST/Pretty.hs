{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Pretty printers for all 3 dialects and core s-expressions
-- and their corresponding `Show` instances for @AST.Skeleton@ types.

module AST.Pretty
  ( module Exports
  , LPP (..)
  , PPableLIGO
  , Pretty (..)
  , TotalLPP
  , docToText
  , lppDialect
  , sexpr
  ) where

import Data.Kind (Type)
import Data.Maybe (isJust)
import Data.Sum
import Data.Text (Text)
import Data.Text qualified as Text (pack)
import Duplo (Cofree ((:<)), Layers)
import Duplo.Pretty as Exports
  (Doc, Modifies (..), PP (PP), Pretty (..), Pretty1 (..), above, brackets, empty, fsep, indent,
  parens, ppToText, punctuate, ($+$), (<+>), (<.>))
import Duplo.Tree (Tree)

import AST.Skeleton hiding (Type)
import AST.Skeleton qualified as AST
import Parser (LineMarker (..), LineMarkerType (..), ShowRange)
import Product (Contains)
import Range (Range)

----------------------------------------------------------------------------
-- Internal
----------------------------------------------------------------------------

class (Pretty expr) => LPP (dialect :: Lang) expr where
    lpp :: expr -> Doc
    lpp = pp

instance LPP dialect () where
instance LPP dialect Text where
instance LPP dialect Doc where

class (Pretty1 expr) => LPP1 (dialect :: Lang) (expr :: Type -> Type) where
  lpp1 :: expr Doc -> Doc
  lpp1 = pp1

instance LPP1 dialect [] where
  lpp1 = list

deriving anyclass instance LPP1 dialect Maybe

instance {-# OVERLAPPABLE #-}
    (LPP d a, LPP1 d p, Functor p)
  =>
    LPP d (p a)
  where
  lpp = lpp1 @d . fmap (lpp @d)

deriving via PP (RawContract it) instance Pretty it => Show (RawContract it)
deriving via PP (Binding it) instance Pretty it => Show (Binding it)
deriving via PP (AST.Type it) instance Pretty it => Show (AST.Type it)
deriving via PP (Variant it) instance Pretty it => Show (Variant it)
deriving via PP (TField it) instance Pretty it => Show (TField it)
deriving via PP (Expr it) instance Pretty it => Show (Expr it)
deriving via PP (Alt it) instance Pretty it => Show (Alt it)
deriving via PP (MapBinding it) instance Pretty it => Show (MapBinding it)
deriving via PP (FieldAssignment it) instance Pretty it => Show (FieldAssignment it)
deriving via PP (Constant it) instance Pretty it => Show (Constant it)
deriving via PP (Pattern it) instance Pretty it => Show (Pattern it)
deriving via PP (ModuleAccess it) instance Pretty it => Show (ModuleAccess it)
deriving via PP (QualifiedName it) instance Pretty it => Show (QualifiedName it)
deriving via PP (Name it) instance Pretty it => Show (Name it)
deriving via PP (NameDecl it) instance Pretty it => Show (NameDecl it)
deriving via PP (ModuleName it) instance Pretty it => Show (ModuleName it)
deriving via PP (TypeName it) instance Pretty it => Show (TypeName it)
deriving via PP (Ctor it) instance Pretty it => Show (Ctor it)
deriving via PP (FieldName it) instance Pretty it => Show (FieldName it)
deriving via PP (Error it) instance Pretty it => Show (Error it)

instance
    ( Apply (LPP1 d) layers
    , Apply Pretty1 layers
    )
  =>
    LPP1 d (Sum layers)
  where
  lpp1 = apply @(LPP1 d) (lpp1 @d)

instance
    ( Layers layers
    , Modifies info
    , Apply (LPP1 d) layers
    )
  =>
    LPP d (Tree layers info)
  where
  lpp (d :< f) = ascribe d $ lpp1 @d $ lpp @d <$> f

instance LPP1 d Error where
  lpp1 (Error msg _) = lpp @d msg

-- class LPPProd (dialect :: Lang) xs where
--   lppProd :: Product xs -> Doc

-- instance {-# OVERLAPS #-} LPP d x => LPPProd d '[x] where
--   lppProd (x :> Nil) = lpp @d x

-- instance (LPP d x, LPPProd d xs) => LPPProd d (x : xs) where
--   lppProd (x :> xs) = "testprod"

-- instance LPP d (Product '[]) where
--   lpp Nil = "emptyprod"

-- instance (LPPProd d xs, PrettyProd xs) => LPP d (Product xs) where
--   lpp _ = "lpprodi"

-- instance LPP d Range where
-- instance LPP d ShowRange where

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

sexpr :: Text -> [Doc] -> Doc
sexpr header items = "(" <.> pp header `indent` foldr above empty items <.> ")"

sop :: Doc -> Text -> [Doc] -> Doc
sop a op b = "(" <.> a `indent` pp op `indent` foldr above empty b <.> ")"

blockWith :: forall dialect p . LPP dialect p => (Doc -> Doc) -> [p] -> Doc
blockWith f = foldr (indent . f . lpp @dialect) empty

block' :: [Doc] -> Doc
-- block' = foldr (<.>) empty . map ((<.> "\n") . lpp)
block' = foldr (($+$) . (<.> "\n") . lpp) empty

list :: forall dialect p . LPP dialect p => [p] -> Doc
list = brackets . train @dialect @p ";"

train :: forall dialect p . LPP dialect p => Doc -> [p] -> Doc
train sep' = fsep . punctuate sep' . map (lpp @dialect)

tuple :: forall dialect p . LPP dialect p => [p] -> Doc
tuple = parens . train @dialect @p ","

braces :: Doc -> Doc
braces p = "{" <+> p <+> "}"

----------------------------------------------------------------------------
-- Core sexpr
----------------------------------------------------------------------------

instance Pretty1 RawContract where
  pp1 = \case
    RawContract xs -> sexpr "contract" xs

instance Pretty1 Binding where
  pp1 = \case
    BTypeDecl     n    ty       -> sexpr "type_decl"  [n, ty]
    BParameter    n    ty       -> sexpr "parameter"  [n, pp ty]
    BVar          name ty value -> sexpr "var"   [name, pp ty, pp value]
    BConst        name ty body  -> sexpr "const" [name, pp ty, pp body]
    BAttribute    name          -> sexpr "attr"  [name]
    BInclude      fname         -> sexpr "#include" [fname]
    BImport       fname alias   -> sexpr "#import" [fname, alias]

    BFunction isRec name params ty body ->
      sexpr "fun" $ concat
        [ ["rec" | isRec]
        , [name]
        , params
        , [":", pp ty]
        , ["=", body]
        ]

instance Pretty1 AST.Type where
  pp1 = \case
    TArrow    dom codom -> sop dom "->" [codom]
    TRecord   fields    -> sexpr "RECORD" fields
    TSum      variants  -> sexpr "SUM" variants
    TProduct  elements  -> sexpr "PROD" elements
    TApply    f xs      -> sop f "$" xs
    TString   t         -> sexpr "TSTRING" [pp t]
    TOr       l n r m   -> sexpr "OR"   [l, n, r, m]
    TAnd      l n r m   -> sexpr "AND" [l, n, r, m]
    TWildcard           -> "_"

instance Pretty1 Variant where
  pp1 = \case
    Variant ctor ty -> sexpr "ctor" [ctor, pp ty]

instance Pretty1 Expr where
  pp1 = \case
    Let       decl body  -> "(let" `indent` decl `above` body <.> ")"
    Apply     f xs       -> sexpr "apply" (f : xs)
    Constant  constant   -> sexpr "constant" [constant]
    Ident     qname      -> sexpr "qname" [qname]
    BinOp     l o r      -> sop l (ppToText o) [r]
    UnOp        o r      -> sexpr (ppToText o) [r]
    Op          o        -> pp o
    Record    az         -> sexpr "record" az
    If        b t e      -> sexpr "if" [b, t, pp e]
    Assign    l r        -> sop l ":=" [r]
    List      l          -> sexpr "list" l
    ListAccess l ids     -> sexpr "get" (l : ids)
    Set       l          -> sexpr "set" l
    Tuple     l          -> sexpr "tuple" l
    Annot     n t        -> sop n ":" [t]
    Attrs     ts         -> sexpr "attrs" ts
    BigMap    bs         -> sexpr "big_map" bs
    Map       bs         -> sexpr "map" bs
    MapRemove k m        -> sexpr "remove_map" [k, m]
    SetRemove k s        -> sexpr "remove_set" [k, s]
    -- Indexing  a j        -> sexpr "index" [a, j]
    Case      s az       -> sexpr "case" (s : az)
    Skip                 -> "skip"
    ForLoop   j s f d b  -> sexpr "for" [j, s, f, pp d, b]
    ForBox    k mv t z b -> sexpr "for_box" [k, pp mv, pp t, z, b]
    WhileLoop f b        -> sexpr "while" [f, b]
    Seq       es         -> sexpr "seq" es
    Block     es         -> sexpr "block" es
    Lambda    ps ty b    -> sexpr "lam" $ concat [ps, [":", pp ty], ["=>", b]]
    MapPatch  z bs       -> sexpr "patch" (z : bs)
    SetPatch  z bs       -> sexpr "patch_set" (z : bs)
    RecordUpd r up       -> sexpr "update" (r : up)
    Michelson c t args   -> sexpr "%Michelson" (c : t : args)
    Paren     e          -> "(" <> pp e <> ")"

instance Pretty1 Collection where
  pp1 = \case
    CList -> "list"
    CMap  -> "map"
    CSet  -> "set"

instance Pretty1 MichelsonCode where
  pp1 = \case
    MichelsonCode _ -> "<SomeMichelsonCode>"

instance Pretty1 Alt where
  pp1 = \case
    Alt p b -> sexpr "alt" [p, b]

instance Pretty1 MapBinding where
  pp1 = \case
    MapBinding k v -> sexpr "bind" [k, v]

instance Pretty1 FieldAssignment where
  pp1 = \case
    FieldAssignment accessors e -> sexpr ".=" (accessors <> [e])
    Spread n -> sexpr "..." [n]
    Capture accessors -> sexpr ".=" accessors

instance Pretty1 Constant where
  pp1 = \case
    Int           z   -> pp z
    Nat           z   -> pp z
    String        z   -> pp z
    Float         z   -> pp z
    Bytes         z   -> pp z
    Tez           z   -> pp z

instance Pretty1 ModuleAccess where
  pp1 = \case
    ModuleAccess path field -> sexpr "." (path <> [field])

instance Pretty1 QualifiedName where
  pp1 = \case
    QualifiedName src path -> sexpr "." (src : path)

instance Pretty1 Pattern where
  pp1 = \case
    IsConstr     ctor arg  -> sexpr "ctor?" [ctor, pp arg]
    IsConstant   z         -> sexpr "is?" [z]
    IsVar        name      -> sexpr "?" [name]
    IsCons       h t       -> sop h "::?" [t]
    IsAnnot      s t       -> sexpr "type?" [s, t]
    IsWildcard             -> "_?"
    IsSpread     n         -> "..." <.> pp n
    IsList       l         -> sexpr "list?" l
    IsTuple      t         -> sexpr "tuple?" t
    IsRecord     xs        -> sexpr "record?" xs
    IsParen      x         -> "(?" <> pp x <> ")"

instance Pretty1 RecordFieldPattern where
  pp1 = \case
    IsRecordField l b -> sexpr "rec_field?" [l, b]
    IsRecordCapture l -> sexpr "rec_capture?" [l]

instance Pretty1 Preprocessor where
  pp1 = \case
    Preprocessor p -> sexpr "preprocessor" [p]

instance Pretty1 PreprocessorCommand where
  pp1 = \case
    PreprocessorCommand command -> pp command

instance Pretty1 Name where
  pp1 = \case
    Name         raw -> pp raw

instance Pretty1 NameDecl where
  pp1 = \case
    NameDecl     raw -> pp raw

instance Pretty1 ModuleName where
  pp1 = \case
    ModuleName   raw -> pp raw

instance Pretty1 TypeName where
  pp1 = \case
    TypeName     raw -> pp raw

instance Pretty1 FieldName where
  pp1 = \case
    FieldName    raw -> pp raw

instance Pretty1 Ctor where
  pp1 = \case
    Ctor         raw -> pp raw

instance Pretty1 TField where
  pp1 = \case
    TField      n t -> n <.> ":" `indent` t

instance Pretty1 Error where
  pp1 = \case
    Error src children -> sexpr "ERROR" ["\"" <> pp src <> "\"", pp children]

instance Pretty LineMarker where
  pp (LineMarker fp f l _) = sexpr "#" [pp l, pp $ Text.pack fp, pp f]

instance Pretty LineMarkerType where
  pp RootFile     = ""
  pp IncludedFile = "1"
  pp ReturnToFile = "2"

----------------------------------------------------------------------------
-- Common
----------------------------------------------------------------------------

instance LPP1 d ModuleAccess where
  lpp1 = \case
    ModuleAccess path field -> mconcat (punctuate "." path) <> lpp field

instance LPP1 d QualifiedName where
  lpp1 = \case
    QualifiedName src path -> mconcat $ punctuate "." (src : path)

instance LPP1 d RawContract where
  lpp1 = \case
    RawContract xs -> block' xs

instance LPP1 d Name where
  lpp1 = \case
    Name         raw -> lpp raw

instance LPP1 d NameDecl where
  lpp1 = \case
    NameDecl     raw -> lpp raw

instance LPP1 d ModuleName where
  lpp1 = \case
    ModuleName   raw -> lpp raw

instance LPP1 d TypeName where
  lpp1 = \case
    TypeName     raw -> lpp raw

instance LPP1 d FieldName where
  lpp1 = \case
    FieldName    raw -> lpp raw

instance LPP1 d Ctor where
  lpp1 = \case
    Ctor         raw -> lpp raw

instance LPP1 d Preprocessor where
  lpp1 = pp1

instance LPP1 d PreprocessorCommand where
  lpp1 = pp1

instance LPP1 d MichelsonCode where
  lpp1 = pp

--instance LPP1 d LineMarker where
--  lpp1 = pp1

-- instances needed to pass instance resolution during compilation

instance LPP1 d Collection where
  lpp1 = \case
    CList -> "list"
    CMap  -> "map"
    CSet  -> "set"

instance LPP1 'Caml MapBinding where
  lpp1 = error "unexpected `MapBinding` node"

----------------------------------------------------------------------------
-- Pascal
----------------------------------------------------------------------------

instance LPP1 'Pascal AST.Type where
  lpp1 = \case
    TArrow    dom codom -> dom <+> "->" <+> codom
    TRecord   fields    -> "record [" `above` blockWith (<.> ";") fields `above` "]"
    TProduct  [element] -> element
    TProduct  elements  -> parens $ train " *" elements
    TSum      (x:xs)    -> x <.> blockWith ("|"<.>) xs
    TSum      []        -> error "looks like you've been given malformed AST" -- never called
    TApply    f xs      -> f <+> tuple xs
    TString   t         -> "\"" <.> lpp t <.> "\""
    TOr       l n r m   -> tuple [l, n, r, m]
    TAnd      l n r m   -> tuple [l, n, r, m]
    TWildcard           -> "_"

instance LPP1 'Pascal Binding where
  lpp1 = \case
    BTypeDecl     n    ty       -> "type" <+> lpp n <+> "is" <+> lpp ty
    BVar          name ty value -> "var" <+> name <+> ":" <+> lpp ty <+> ":=" <+> lpp value
    BConst        name ty body  -> "const" <+> name <+> ":" <+> lpp ty <+> "=" <+> lpp body
    BAttribute    name          -> brackets ("@" <.> name)
    BInclude      fname         -> "#include" <+> pp fname
    BImport       fname alias   -> "#import" <+> pp fname <+> pp alias
    BParameter    n t           -> "const" <+> n <+> ":" <+> lpp t

    BFunction _ name params ty body ->
      foldr (<+>) empty $ concat
        [ ["function"]
        , [name]
        , params
        , [":", lpp ty]
        , ["is", body]
        ]

instance LPP1 'Pascal Variant where
  lpp1 = \case -- We prepend "|" in sum type itself to be aware of the first one
    Variant ctor ty -> ctor <+> "of" <+> pp ty

instance LPP1 'Pascal Expr where
  lpp1 = \case
    Let       decl body  -> "block {" `above` decl `above` "}" <+> "with" <+> body
    Apply     f xs       -> f <+> tuple xs
    Constant  constant   -> constant
    Ident     qname      -> qname
    BinOp     l o r      -> l <+> o <+> r
    UnOp        o r      -> o <+> r
    Op          o        -> lpp o
    Record    az         -> "record [" `indent` blockWith (<.> ";") az `above` "]"
    -- TODO: context-dependent block expressions
    If        b t e      -> "if" <+> parens b <+> "then {" <+> lpp t <+> "} else {" <+> lpp e <+> "}"
    Assign    l r        -> l <+> ":=" <+> r
    List      []         -> "nil"
    List      l          -> lpp l
    ListAccess l ids     -> lpp l <.> fsep (brackets <$> ids)
    Set       l          -> "set [" <+> lpp l <+> "]"
    Tuple     l          -> tuple l
    Annot     n t        -> parens $ n <+> ":" <+> t
    BigMap    bs         -> "big_map [" `indent` train ";" bs `above` "]"
    Map       bs         -> "map [" `indent` train ";" bs `above` "]"
    MapRemove k m        -> "remove" <+> k <+> "from map" <+> m
    SetRemove k s        -> "remove" <+> k <+> "from set" <+> s
    Skip                 -> "skip"
    ForLoop   j s f d b  -> foldr (<+>) empty
      [ "for", j, ":=", lpp s
      , "to", lpp f, "block' {", lpp d, "} with", lpp b
      ]
    ForBox    k mv t z b -> foldr (<+>) empty
      [ "for", k
      , maybe empty ("->" <+>) mv
      , "in", lpp t
      , z, lpp b
      ]
    WhileLoop f b        -> "while" <+> lpp f <+> "block' {" `indent` lpp b `above` "}"
    Seq       es         -> block' $ map (<.>";") es
    Attrs     ts         -> mconcat $ brackets . ("@"<+>) <$> ts
    Lambda    ps ty b    -> "function" <+> lpp ps <+> ":" <+> lpp ty <+> "is" <+> lpp b
    MapPatch  z bs       -> "patch" <+> z <+> "with map" <+> lpp bs
    SetPatch  z bs       -> "patch" <+> z <+> "with set" <+> lpp bs
    RecordUpd r up       -> r <+> "with record" <+> lpp up
    Case      s az       -> foldr (<+>) empty
      [ "case"
      , lpp s
      , "of\n"
      , foldr above empty $ lpp <$> az
      , "end"
      ]
    Paren     e          -> "(" <+> lpp e <+> ")"
    node                 -> error "unexpected `Expr` node failed with: " <+> pp node

instance LPP1 'Pascal Alt where
  lpp1 = \case
    Alt p b -> "|" <+> lpp p <+> "->" <+> lpp b

instance LPP1 'Pascal FieldAssignment where
  lpp1 = \case
    FieldAssignment n e -> lpp n <+> "=" <+> lpp e
    Spread n -> "..." <+> n
    Capture n -> lpp n

instance LPP1 'Pascal Constant where
  lpp1 = \case
    Int           z   -> lpp z
    Nat           z   -> lpp z
    String        z   -> lpp z
    Float         z   -> lpp z
    Bytes         z   -> lpp z
    Tez           z   -> lpp z

instance LPP1 'Pascal Pattern where
  lpp1 = \case
    IsConstr     ctor arg  -> ctor <+> lpp arg
    IsVar        name      -> name
    IsCons       h t       -> h <+> "::" <+> t
    IsAnnot      s t       -> parens (lpp s <+> ":" <+> lpp t)
    IsList       []        -> "nil"
    IsList       l         -> list l
    IsTuple      t         -> tuple t
    IsParen      x         -> parens x
    pat                    -> error "unexpected `Pattern` node failed with: " <+> pp pat

instance LPP1 'Pascal RecordFieldPattern where
  lpp1 = \case
    IsRecordField name body -> name <+> "=" <+> body
    IsRecordCapture name -> name

instance LPP1 'Pascal TField where
  lpp1 = \case
    TField      n t -> n <.> ":" `indent` t

instance LPP1 'Pascal MapBinding where
  lpp1 = \case
    MapBinding k v -> lpp k <+> "->" <+> lpp v

----------------------------------------------------------------------------
-- Reason
----------------------------------------------------------------------------

instance LPP1 'Reason AST.Type where
  lpp1 = \case
    TArrow    dom codom -> dom <+> "=>" <+> codom
    TRecord   fields    -> "{" `indent` blockWith (<.> ",") fields `above` "}"
    TProduct  elements  -> tuple elements
    TSum      (x:xs)    -> x <.> blockWith ("| "<.>) xs
    TSum      []        -> error "malformed TSum type" -- never called
    TApply    f xs      -> f <+> tuple xs
    TString   t         -> "\"" <.> lpp t <.> "\""
    TOr       l n r m   -> tuple [l, n, r, m]
    TAnd      l n r m   -> tuple [l, n, r, m]
    TWildcard           -> "_"

instance LPP1 'Reason Binding where
  lpp1 = \case
    BTypeDecl     n    ty       -> "type" <+> n <+> "=" <+> lpp ty
    BConst        name ty body  -> foldr (<+>) empty
      [ "let", name, if isJust ty then ":" <+> lpp ty else "", "=", lpp body, ";" ] -- TODO: maybe append ";" to *all* the expressions in the contract
    BAttribute    name          -> brackets ("@" <.> name)
    BInclude      fname         -> "#include" <+> pp fname
    BImport       fname alias   -> "#import" <+> pp fname <+> pp alias
    BParameter    name ty       -> pp name <> if isJust ty then ":" <+> lpp ty else ""
    node                        -> error "unexpected `Binding` node failed with: " <+> pp node

instance LPP1 'Reason Variant where
  lpp1 = \case -- We prepend "|" in sum type itself to be aware of the first one
    Variant ctor ty -> ctor <+> parens (lpp ty)

instance LPP1 'Reason Expr where
  lpp1 = \case
    Let       decl body  -> "let" `indent` decl `above` "in" <+> body
    Apply     f xs       -> f <+> tuple xs
    Ident     qname      -> qname
    BinOp     l o r      -> l <+> o <+> r
    UnOp        o r      -> lpp o <+> lpp r
    Op          o        -> lpp o
    Record    az         -> "{" `indent` blockWith (<.> ",") az `above` "}"
    If        b t e      -> "if" <+> b <+> braces (lpp t) <+> "else" <+> lpp e -- TODO: primitive return values should be enclosed in braces
    List      l          -> lpp l
    ListAccess l ids     -> lpp l <.> fsep (brackets <$> ids)
    Tuple     l          -> tuple l
    Annot     n t        -> parens (n <+> ":" <+> t)
    Case      s az       -> foldr (<+>) empty
      [ "switch"
      , lpp s
      , "{\n"
      , foldr above empty $ lpp <$> az
      , "\n}"
      ]
    Seq       es         -> train " " es
    Lambda    ps ty b    -> foldr (<+>) empty
      [ tuple ps, if isJust ty then ":" <+> lpp ty else "", "=> {", lpp b, "}" ]
    Paren     e          -> "(" <+> lpp e <+> ")"
    node                 -> error "unexpected `Expr` node failed with: " <+> pp node

instance LPP1 'Reason Alt where
  lpp1 = \case
    Alt p b -> "|" <+> lpp p <+> "=>" <+> lpp b

instance LPP1 'Reason FieldAssignment where
  lpp1 = \case
    FieldAssignment n e -> lpp n <+> "=" <+> lpp e
    Spread n -> "..." <.> n
    Capture n -> lpp n

instance LPP1 'Reason Constant where
  lpp1 = \case
    Int           z   -> lpp z
    Nat           z   -> lpp z
    String        z   -> lpp z
    Float         z   -> lpp z
    Bytes         z   -> lpp z
    Tez           z   -> lpp z

instance LPP1 'Reason Pattern where
  lpp1 = \case
    IsConstr     ctor arg  -> ctor <+> lpp arg
    IsVar        name      -> name
    IsAnnot      s t       -> parens (lpp s <+> ":" <+> lpp t)
    IsWildcard             -> "_"
    IsSpread     n         -> "..." <.> lpp n
    IsList       l         -> brackets $ train "," l
    IsTuple      t         -> train "," t
    IsRecord     fields    -> "{" <+> train "," fields <+> "}"
    IsParen      x         -> parens x
    pat                    -> error "unexpected `Pattern` node failed with: " <+> pp pat

instance LPP1 'Reason RecordFieldPattern where
  lpp1 = \case
    IsRecordField name body -> name <+> "=" <+> body
    IsRecordCapture name -> name

instance LPP1 'Reason TField where
  lpp1 = \case
    TField      n t -> n <.> ":" `indent` t

instance LPP1 'Reason MapBinding where
  lpp1 = \case
    MapBinding k v -> lpp k <+> "->" <+> lpp v

----------------------------------------------------------------------------
-- Caml
----------------------------------------------------------------------------

tupleCameLIGO :: LPP 'Caml p => [p] -> Doc
tupleCameLIGO = \case
  [x] -> lpp @'Caml x
  xs  -> tuple @'Caml xs

instance LPP1 'Caml AST.Type where
  lpp1 = \case
    TArrow    dom codom -> dom <+> "->" <+> codom
    TRecord   fields    -> "{" `indent` blockWith (<.> ";") fields `above` "}"
    TProduct  elements  -> train " *" elements
    TSum      (x:xs)    -> x <.> blockWith ("| "<.>) xs
    TSum      []        -> error "malformed TSum type" -- never called
    TApply    f xs      -> tupleCameLIGO xs <+> f
    TString   t         -> "\"" <.> lpp t <.> "\""
    TOr       l n r m   -> tuple [l, n, r, m]
    TAnd      l n r m   -> tuple [l, n, r, m]
    TWildcard           -> "_"

instance LPP1 'Caml Binding where
  lpp1 = \case
    BTypeDecl     n    ty       -> "type" <+> n <+> "=" <+> lpp ty
    BConst        name ty body  -> "let" <+> name <+> ":" <+> lpp ty <+> lpp body
    BInclude      fname         -> "#include" <+> pp fname
    BImport       fname alias   -> "#import" <+> pp fname <+> pp alias

    BFunction isRec name params ty body ->
      foldr (<+>) empty $ concat
        [ ["let"]
        , ["rec" | isRec]
        , [name]
        , params
        , [if isJust ty then ":" <+> lpp ty else empty]
        , ["=", body]
        ]
    node                      -> error "unexpected `Binding` node failed with: " <+> pp node

instance LPP1 'Caml Variant where
  lpp1 = \case -- We prepend "|" in sum type itself to be aware of the first one
    Variant ctor ty -> ctor <+> ":" <+> pp ty

instance LPP1 'Caml Expr where
  lpp1 = \case
    Let       decl body  -> "let" `indent` decl `above` "in" <+> body
    Apply     f xs       -> f <+> train " " xs
    Ident     qname      -> qname
    BinOp     l o r      -> l <+> o <+> r
    UnOp        o r      -> lpp o <+> lpp r
    Op          o        -> lpp o
    Record    az         -> "{" `indent` blockWith (<.> ";") az `above` "}"
    If        b t e      -> "if" <+> b <+> "then" <+> lpp t <+> "else" <+> lpp e
    List      l          -> lpp l
    ListAccess l ids     -> lpp l <.> fsep (brackets <$> ids)
    Tuple     l          -> tuple l
    Annot     n t        -> parens (n <+> ":" <+> t)
    Case      s az       -> foldr (<+>) empty
      [ "match"
      , lpp s
      , "with\n"
      , foldr above empty $ lpp <$> az
      ]
    Seq       es         -> train " " es
    Lambda    ps ty b    -> foldr (<+>) empty
      [ train "," ps, if isJust ty then ":" <+> lpp ty else "", "=>", lpp b ]
    RecordUpd r with     -> r <+> "with" <+> train ";" with
    Paren     e          -> "(" <+> lpp e <+> ")"
    node                 -> error "unexpected `Expr` node failed with: " <+> pp node

instance LPP1 'Caml Alt where
  lpp1 = \case
    Alt p b -> "|" <+> lpp p <+> "->" <+> lpp b

instance LPP1 'Caml FieldAssignment where
  lpp1 = \case
    FieldAssignment n e -> lpp n <+> "=" <+> lpp e
    Spread n -> "..." <.> n
    Capture n -> lpp n

instance LPP1 'Caml Constant where
  lpp1 = \case
    Int           z   -> lpp z
    Nat           z   -> lpp z
    String        z   -> lpp z
    Float         z   -> lpp z
    Bytes         z   -> lpp z
    Tez           z   -> lpp z

instance LPP1 'Caml Pattern where
  lpp1 = \case
    IsConstr     ctor arg  -> ctor <+> lpp arg
    IsVar        name      -> name
    IsAnnot      s t       -> parens (lpp s <+> ":" <+> lpp t)
    IsWildcard             -> "_"
    IsSpread     n         -> "..." <.> lpp n
    IsList       l         -> list l
    IsTuple      t         -> train "," t
    IsCons       h t       -> h <+> "::" <+> t
    IsRecord     fields    -> "{" <+> train "," fields <+> "}"
    IsParen      x         -> parens x
    pat                    -> error "unexpected `Pattern` node failed with:" <+> pp pat

instance LPP1 'Caml RecordFieldPattern where
  lpp1 = \case
    IsRecordField name body -> name <+> "=" <+> body
    IsRecordCapture name -> name

instance LPP1 'Caml TField where
  lpp1 = \case
    TField      n t -> n <.> ":" `indent` t

type TotalLPP expr = (LPP 'Pascal expr, LPP 'Caml expr, LPP 'Reason expr)

lppDialect :: TotalLPP expr => Lang -> expr -> Doc
lppDialect dialect = case dialect of
  Pascal -> lpp @'Pascal
  Caml -> lpp @'Caml
  Reason -> lpp @'Reason

docToText :: Doc -> Text
docToText = Text.pack . show

type PPableLIGO info =
  ( Contains [Text] info
  , Contains Range info
  , Contains ShowRange info
  )
