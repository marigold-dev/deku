-- | Parser for a PascaLigo contract.
module AST.Parser.Pascaligo
  ( recognise
  ) where

import AST.Skeleton

import Duplo.Tree

import ParseTree
import Parser
import Product (Product (Nil, (:>)))

recognise :: SomeRawTree -> ParserM (SomeLIGO Info)
recognise (SomeRawTree dialect rawTree)
  = fmap (SomeLIGO dialect)
  $ flip (descent (error "Pascaligo.recognise")) rawTree
  $ map usingScope
  [ -- Contract
    Descent do
      boilerplate \case
        "source_file" -> RawContract <$> fields "declaration"
        _ -> fallthrough

    -- Expr
  , Descent do
      boilerplate \case
        "let_expr"          -> Let       <$> field  "locals"    <*> field "body"
        "fun_call"          -> Apply     <$> field  "f"         <*> fields "argument"
        "par_call"          -> Apply     <$> field  "f"         <*> fields "argument"
        "projection_call"   -> Apply     <$> field  "f"         <*> fields "argument"
        "Some_call"         -> Apply     <$> field  "constr"    <*> fields "argument"
        "constr_call"       -> Apply     <$> field  "constr"    <*> fields "argument"
        "arguments"         -> Tuple     <$> fields "argument"
        "unop"              -> UnOp      <$> field  "negate"    <*> field "arg"
        "binop"             -> BinOp     <$> field  "arg1"      <*> field "op"   <*> field "arg2"
        "block"             -> Seq       <$> fields "statement"
        "clause_block"      -> Seq       <$> fields "statement"
        "list_expr"         -> List      <$> fields "element"
        "annot_expr"        -> Annot     <$> field  "subject"   <*> field "type"
        "conditional"       -> If        <$> field  "selector"  <*> field "then" <*> fieldOpt "else"
        "cond_expr"         -> If        <$> field  "selector"  <*> field "then" <*> fieldOpt "else"
        "assignment"        -> Assign    <$> field  "LHS"       <*> field "RHS"
        "record_expr"       -> Record    <$> fields "assignment"
        "big_map_injection" -> BigMap    <$> fields "binding"
        "map_remove"        -> MapRemove <$> field  "key"       <*> field "container"
        "tuple_expr"        -> Tuple     <$> fields "element"
        "skip"              -> return Skip
        "case_expr"         -> Case      <$> field  "subject"    <*> fields   "case"
        "case_instr"        -> Case      <$> field  "subject"    <*> fields   "case"
        "fun_expr"          -> Lambda    <$> fields "parameter"  <*> fieldOpt    "type"  <*> field "body"
        "for_cycle"         -> ForLoop   <$> field  "name"       <*> field    "begin" <*> field "end" <*> fieldOpt "step" <*> field "body"
        "for_box"           -> ForBox    <$> field  "key"        <*> fieldOpt "value" <*> field "kind"  <*> field "collection" <*> field "body"
        "while_loop"        -> WhileLoop <$> field  "breaker"    <*> field    "body"
        "map_injection"     -> Map       <$> fields "binding"
        "list_injection"    -> List      <$> fields "element"
        "set_expr"          -> Set       <$> fields "element"
        "map_patch"         -> MapPatch  <$> field  "container"  <*> fields "binding"
        "set_patch"         -> SetPatch  <$> field  "container"  <*> fields "key"
        "set_remove"        -> SetRemove <$> field  "key"        <*> field  "container"
        "update_record"     -> RecordUpd <$> field  "record"     <*> fields "assignment"
        "michelson_interop" -> Michelson <$> field  "code"       <*> field  "type"    <*> fields "argument"
        "paren_expr"        -> Paren     <$> field  "expr"
        _                   -> fallthrough

    -- Collection
  , Descent do
      boilerplate' \case
        ("collection", "map")  -> pure CMap
        ("collection", "set")  -> pure CSet
        ("collection", "list") -> pure CList
        _                      -> fallthrough

    -- Pattern
  , Descent do
      boilerplate \case
        "user_constr_pattern" -> IsConstr <$> field  "constr" <*> fieldOpt "arguments"
        "tuple_pattern"       -> IsTuple  <$> fields "element"
        "nil"                 -> return $ IsList []
        "list_pattern"        -> IsList   <$> fields "element"
        "cons_pattern"        -> IsCons   <$> field  "head"   <*> field "tail"
        "var_pattern"         -> IsVar    <$> field  "name"
        "record_pattern"      -> IsRecord <$> fields "field"
        "wildcard_pattern"    -> pure IsWildcard
        _                     -> fallthrough

    -- Irrefutable tuple
  , Descent do
      boilerplate $ \case
        "irrefutable_tuple" -> IsTuple <$> fields "item"
        _                   -> fallthrough

    -- RecordFieldPattern
  , Descent do
      boilerplate $ \case
        "record_field_pattern"   -> IsRecordField <$> field "name" <*> field "body"
        "record_capture_pattern" -> IsRecordCapture <$> field "name"
        _                        -> fallthrough

    -- Alt
  , Descent do
      boilerplate \case
        "case_clause_expr"  -> Alt <$> field "pattern" <*> field  "body"
        "case_clause_instr" -> Alt <$> field "pattern" <*> field  "body"
        _                   -> fallthrough

    -- Preprocessor
  , Descent do
      boilerplate \case
        "preprocessor" -> Preprocessor <$> field "preprocessor_command"
        _              -> fallthrough

    -- ProcessorCommand
  , Descent do
      boilerplate' \case
        ("p_if"      , rest) -> return $ PreprocessorCommand $ "#if "      <> rest
        ("p_error"   , rest) -> return $ PreprocessorCommand $ "#error "   <> rest
        ("p_warning" , rest) -> return $ PreprocessorCommand $ "#warning " <> rest
        ("p_define"  , rest) -> return $ PreprocessorCommand $ "#define "  <> rest
        _                    -> fallthrough

    -- FieldAssignment
  , Descent do
      boilerplate \case
        "field_path_assignment" -> FieldAssignment <$> fields "accessor" <*> field "_rhs"
        _                       -> fallthrough

    -- MapBinding
  , Descent do
      boilerplate \case
        "binding" -> MapBinding <$> field "key" <*> field "value"
        _         -> fallthrough

  , Descent do
      boilerplate' \case
        ("negate",     op) -> return $ Op op
        ("adder",      op) -> return $ Op op
        ("multiplier", op) -> return $ Op op
        ("comparison", op) -> return $ Op op
        ("^",          _)  -> return $ Op "^"
        ("#",          _)  -> return $ Op "#"
        _                  -> fallthrough

  , Descent do
      boilerplate \case
        "data_projection" -> QualifiedName <$> field "struct" <*> fields "accessor"
        "map_lookup"      -> QualifiedName <$> field "container" <*> fields "index"
        "module_field"    -> QualifiedName <$> field "module" <*> fields "method"
        _                 -> fallthrough

    -- Literal
  , Descent do
      boilerplate' \case
        ("Int",    i) -> return $ Int i
        ("Nat",    i) -> return $ Nat i
        ("Bytes",  i) -> return $ Bytes i
        ("String", i) -> return $ String i
        ("Tez",    i) -> return $ Tez i
        _             -> fallthrough

    -- Declaration
  , Descent do
      boilerplate \case
        "fun_decl"   -> BFunction <$> flag "recursive" <*> field "name" <*> fields "parameter" <*> fieldOpt "type" <*> field "body"
        "const_decl" -> BConst    <$>             field    "name"       <*> fieldOpt "type" <*> fieldOpt "value"
        "var_decl"   -> BVar      <$>             field    "name"       <*> fieldOpt "type" <*> fieldOpt "value"
        "type_decl"  -> BTypeDecl <$>             field    "typeName"   <*> field "typeValue"
        "p_include"  -> BInclude  <$>             field    "filename"
        "p_import"   -> BImport   <$>             field    "filename" <*> field "alias"
        _            -> fallthrough

    -- VarDecl
  , Descent do
      boilerplate \case
        "param_decl" -> BParameter <$> field "name" <*> fieldOpt "type"
        _            -> fallthrough

  --   -- Mutable
  -- , Descent do
  --     boilerplate \case
  --       "const" -> return Immutable
  --       "var"   -> return Mutable
  --       _       -> fallthrough

    -- Name
  , Descent do
      boilerplate' \case
        ("Name", n)     -> return $ Name n
        ("and", _)      -> return $ Name "and"
        ("or", _)       -> return $ Name "or"
        ("contains", _) -> return $ Name "contains"
        _               -> fallthrough

    -- NameDecl
  , Descent do
      boilerplate' $ \case
        ("NameDecl", n) -> return $ NameDecl n
        _               -> fallthrough

    -- ModuleName
  , Descent do
      boilerplate' $ \case
        ("ModuleName", n) -> return $ ModuleName n
        _                 -> fallthrough

    -- Type
  , Descent do
      boilerplate \case
        "fun_type"         -> TArrow   <$> field  "domain" <*> field "codomain"
        "prod_type"        -> TProduct <$> fields "element"
        "app_type"         -> TApply   <$> field  "name" <*> fields "arg"
        "record_type"      -> TRecord  <$> fields "field"
        "sum_type"         -> TSum     <$> fields "variant"
        "michelsonTypeOr"  -> TOr      <$> field "left_type" <*> field "left_type_name" <*> field "right_type" <*> field "right_type_name"
        "michelsonTypeAnd" -> TAnd     <$> field "left_type" <*> field "left_type_name" <*> field "right_type" <*> field "right_type_name"
        "type_group"       -> TProduct <$> (pure <$> field "type")
        "TypeWildcard"     -> pure TWildcard
        _                  -> fallthrough

    -- Module access:
  , Descent do
      boilerplate $ \case
        "module_TypeName" -> ModuleAccess <$> fields "path" <*> field "type"
        "module_access"   -> ModuleAccess <$> fields "path" <*> field "field"
        _                 -> fallthrough

    -- Variant
  , Descent do
      boilerplate \case
        "variant" -> Variant <$> field "constructor" <*> fieldOpt "arguments"
        _         -> fallthrough

  -- MichelsonCode
  , Descent do
      boilerplate' \case
        ("michelson_code", code) -> return $ MichelsonCode code
        _                        -> fallthrough

    -- TField
  , Descent do
      boilerplate \case
        "field_decl" -> TField <$> field "fieldName" <*> field "fieldType"
        _            -> fallthrough

    -- TypeName
  , Descent do
      boilerplate' \case
        ("TypeName", name) -> return $ TypeName name
        _                  -> fallthrough

    -- Ctor
  , Descent do
      boilerplate' \case
        ("ConstrName", name) -> return $ Ctor name
        ("ModuleName", name) -> return $ Ctor name
        ("None", _)          -> return $ Ctor "None"
        ("True", _)          -> return $ Ctor "True"
        ("False", _)         -> return $ Ctor "False"
        ("Unit", _)          -> return $ Ctor "Unit"
        ("constr", n)        -> return $ Ctor n
        _                    -> fallthrough

    -- FieldName
  , Descent do
      boilerplate' \case
        ("FieldName", name) -> return $ FieldName name
        _                   -> fallthrough

    -- Err
  , Descent do
      \(r :> _, ParseTree _ children source) ->
        withComments do
          return ([] :> r :> N :> CodeSource source :> Nil, Error source children)
  ]
