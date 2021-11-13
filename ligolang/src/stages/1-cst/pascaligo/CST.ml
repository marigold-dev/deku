(* Concrete Syntax Tree (CST) for LIGO *)

(* To disable warning about multiply-defined record labels. *)

[@@@warning "-30-40-42"]

(* Vendor dependencies *)

module Directive = LexerLib.Directive
module Utils     = Simple_utils.Utils
module Region    = Simple_utils.Region
module Token     = Lexing_pascaligo.Token

open Utils
type 'a reg = 'a Region.reg

(* Lexemes *)

type lexeme = string

type 'payload wrap = 'payload Token.wrap 

(* Keywords of LIGO *)

type keyword        = lexeme wrap
type kwd_and        = lexeme wrap
type kwd_attributes = lexeme wrap
type kwd_begin      = lexeme wrap
type kwd_block      = lexeme wrap
type kwd_case       = lexeme wrap
type kwd_const      = lexeme wrap
type kwd_contains   = lexeme wrap
type kwd_down       = lexeme wrap
type kwd_else       = lexeme wrap
type kwd_end        = lexeme wrap
type kwd_for        = lexeme wrap
type kwd_from       = lexeme wrap
type kwd_function   = lexeme wrap
type kwd_recursive  = lexeme wrap
type kwd_if         = lexeme wrap
type kwd_in         = lexeme wrap
type kwd_is         = lexeme wrap
type kwd_list       = lexeme wrap
type kwd_map        = lexeme wrap
type kwd_mod        = lexeme wrap
type kwd_nil        = lexeme wrap
type kwd_not        = lexeme wrap
type kwd_of         = lexeme wrap
type kwd_or         = lexeme wrap
type kwd_patch      = lexeme wrap
type kwd_record     = lexeme wrap
type kwd_remove     = lexeme wrap
type kwd_set        = lexeme wrap
type kwd_skip       = lexeme wrap
type kwd_step       = lexeme wrap
type kwd_then       = lexeme wrap
type kwd_to         = lexeme wrap
type kwd_type       = lexeme wrap
type kwd_var        = lexeme wrap
type kwd_while      = lexeme wrap
type kwd_with       = lexeme wrap
type kwd_module    = lexeme wrap

(* Symbols *)

type semi     = lexeme wrap  (* ";"   *)
type comma    = lexeme wrap  (* ","   *)
type lpar     = lexeme wrap  (* "("   *)
type rpar     = lexeme wrap  (* ")"   *)
type lbrace   = lexeme wrap  (* "{"   *)
type rbrace   = lexeme wrap  (* "}"   *)
type lbracket = lexeme wrap  (* "["   *)
type rbracket = lexeme wrap  (* "]"   *)
type cons     = lexeme wrap  (* "#"   *)
type vbar     = lexeme wrap  (* "|"   *)
type arrow    = lexeme wrap  (* "->"  *)
type assign   = lexeme wrap  (* ":="  *)
type equal    = lexeme wrap  (* "="   *)
type colon    = lexeme wrap  (* ":"   *)
type lt       = lexeme wrap  (* "<"   *)
type leq      = lexeme wrap  (* "<="  *)
type gt       = lexeme wrap  (* ">"   *)
type geq      = lexeme wrap  (* ">="  *)
type neq      = lexeme wrap  (* "=/=" *)
type plus     = lexeme wrap  (* "+"   *)
type minus    = lexeme wrap  (* "-"   *)
type slash    = lexeme wrap  (* "/"   *)
type times    = lexeme wrap  (* "*"   *)
type dot      = lexeme wrap  (* "."   *)
type cat      = lexeme wrap  (* "^"   *)

(* Virtual tokens *)

type eof = lexeme wrap

(* Literals *)

type variable    = string reg
type module_name = string reg
type fun_name    = string reg
type type_name   = string reg
type type_var    = string reg
type type_constr = string reg
type field_name  = string reg
type map_name    = string reg
type set_name    = string reg
type constr      = string reg
type attribute   = string reg

(* Parentheses *)

type 'a par = {
  lpar   : lpar;
  inside : 'a;
  rpar   : rpar
}

(* Brackets compounds *)

type 'a brackets = {
  lbracket : lbracket;
  inside   : 'a;
  rbracket : rbracket
}

(* Braced compounds *)

type 'a braces = {
  lbrace : lbrace;
  inside : 'a;
  rbrace : rbrace
}

(* The Abstract Syntax Tree *)

type t = {
  decl : declaration nseq;
  eof  : eof
}

and ast = t

and attributes = attribute list

and declaration =
  TypeDecl    of type_decl    reg
| ConstDecl   of const_decl   reg
| FunDecl     of fun_decl     reg
| ModuleDecl  of module_decl  reg
| ModuleAlias of module_alias reg
| Directive   of Directive.t

and const_decl = {
  kwd_const  : kwd_const;
  pattern    : pattern;
  const_type : (colon * type_expr) option;
  equal      : equal;
  init       : expr;
  terminator : semi option;
  attributes : attributes
}

(* Type declarations *)

and type_decl = {
  kwd_type   : kwd_type;
  name       : type_name;
  params     : type_vars option;
  kwd_is     : kwd_is;
  type_expr  : type_expr;
  terminator : semi option
}

and type_vars = (type_var, comma) nsepseq par reg

and module_decl = {
  kwd_module : kwd_module;
  name       : module_name;
  kwd_is     : kwd_is;
  enclosing  : module_enclosing;
  module_    : t;
  terminator : semi option;
}

and module_alias = {
  kwd_module : kwd_module;
  alias      : module_name;
  kwd_is     : kwd_is;
  binders    : (module_name, dot) nsepseq;
  terminator : semi option
}

and type_expr =
  TProd   of cartesian
| TSum    of sum_type reg
| TRecord of field_decl reg ne_injection reg
| TApp    of (type_constr * type_tuple) reg
| TFun    of (type_expr * arrow * type_expr) reg
| TPar    of type_expr par reg
| TVar    of variable
| TString of lexeme reg
| TInt    of (lexeme * Z.t) reg
| TModA   of type_expr module_access reg

and sum_type = {
  lead_vbar  : vbar option;
  variants   : (variant reg, vbar) nsepseq;
  attributes : attributes
}

and field_decl = {
  field_name : field_name;
  colon      : colon;
  field_type : type_expr;
  attributes : attributes
}

and cartesian = (type_expr, times) nsepseq reg

and variant = {
  constr     : constr;
  arg        : (kwd_of * type_expr) option;
  attributes : attributes
}

and type_tuple = (type_expr, comma) nsepseq par reg

(* Function and procedure declarations *)

and fun_expr = {
  kwd_function : kwd_function;
  param        : parameters;
  ret_type     : (colon * type_expr) option;
  kwd_is       : kwd_is;
  return       : expr;
  attributes  : attributes
}

and fun_decl = {
  kwd_recursive : kwd_recursive option;
  kwd_function  : kwd_function;
  fun_name      : variable;
  param         : parameters;
  ret_type      : (colon * type_expr) option;
  kwd_is        : kwd_is;
  return        : expr;
  terminator    : semi option;
  attributes    : attributes
}

and block_with = {
  block    : block reg;
  kwd_with : kwd_with;
  expr     : expr
}

and parameters = (param_decl, semi) nsepseq par reg

and param_decl =
  ParamConst of param_const reg
| ParamVar   of param_var reg

and param_const = {
  kwd_const  : kwd_const;
  var        : var_pattern reg;
  param_type : (colon * type_expr) option
}

and param_var = {
  kwd_var    : kwd_var;
  var        : var_pattern reg;
  param_type : (colon * type_expr) option
}

and block = {
  enclosing  : block_enclosing;
  statements : statements;
  terminator : semi option
}

and block_enclosing =
  Block    of kwd_block * lbrace * rbrace
| BeginEnd of kwd_begin * kwd_end

and module_enclosing =
  Brace    of lbrace * rbrace
| BeginEnd of kwd_begin * kwd_end

and statements = (statement, semi) nsepseq

and statement =
  Instr of instruction
| Data  of data_decl

and data_decl =
  LocalConst       of const_decl   reg
| LocalVar         of var_decl     reg
| LocalFun         of fun_decl     reg
| LocalType        of type_decl    reg
| LocalModule      of module_decl  reg
| LocalModuleAlias of module_alias reg

and var_decl = {
  kwd_var    : kwd_var;
  pattern    : pattern;
  var_type   : (colon * type_expr) option;
  assign     : assign;
  init       : expr;
  terminator : semi option;
  attributes : attributes
}

and instruction =
  Cond        of conditional reg
| CaseInstr   of if_clause case reg
| Assign      of assignment reg
| Loop        of loop
| ProcCall    of fun_call
| Skip        of kwd_skip
| RecordPatch of record_patch reg
| MapPatch    of map_patch reg
| SetPatch    of set_patch reg
| MapRemove   of map_remove reg
| SetRemove   of set_remove reg

and set_remove = {
  kwd_remove : kwd_remove;
  element    : expr;
  kwd_from   : kwd_from;
  kwd_set    : kwd_set;
  set        : path
}

and map_remove = {
  kwd_remove : kwd_remove;
  key        : expr;
  kwd_from   : kwd_from;
  kwd_map    : kwd_map;
  map        : path
}

and set_patch  = {
  kwd_patch : kwd_patch;
  path      : path;
  kwd_with  : kwd_with;
  set_inj   : expr ne_injection reg
}

and map_patch  = {
  kwd_patch : kwd_patch;
  path      : path;
  kwd_with  : kwd_with;
  map_inj   : binding reg ne_injection reg
}

and binding = {
  source : expr;
  arrow  : arrow;
  image  : expr
}

and record_patch = {
  kwd_patch  : kwd_patch;
  path       : path;
  kwd_with   : kwd_with;
  record_inj : record reg
}

and cond_expr = {
  kwd_if     : kwd_if;
  test       : expr;
  kwd_then   : kwd_then;
  ifso       : expr;
  terminator : semi option;
  kwd_else   : kwd_else;
  ifnot      : expr
}

and conditional = {
  kwd_if     : kwd_if;
  test       : expr;
  kwd_then   : kwd_then;
  ifso       : if_clause;
  terminator : semi option;
  kwd_else   : kwd_else;
  ifnot      : if_clause
}

and if_clause =
  ClauseInstr of instruction
| ClauseBlock of clause_block

and clause_block =
  LongBlock  of block reg
| ShortBlock of (statements * semi option) braces reg

and set_membership = {
  set          : expr;
  kwd_contains : kwd_contains;
  element      : expr
}

and 'a case = {
  kwd_case  : kwd_case;
  expr      : expr;
  kwd_of    : kwd_of;
  enclosing : enclosing;
  lead_vbar : vbar option;
  cases     : ('a case_clause reg, vbar) nsepseq reg
}

and 'a case_clause = {
  pattern : pattern;
  arrow   : arrow;
  rhs     : 'a
}

and assignment = {
  lhs    : lhs;
  assign : assign;
  rhs    : expr;
}

and lhs =
  Path    of path
| MapPath of map_lookup reg

and loop =
  While of while_loop reg
| For   of for_loop

and while_loop = {
  kwd_while : kwd_while;
  cond      : expr;
  block     : block reg
}

and for_loop =
  ForInt     of for_int reg
| ForCollect of for_collect reg

and for_int = {
  kwd_for    : kwd_for;
  binder     : variable;
  assign     : assign;
  init       : expr;
  kwd_to     : kwd_to;
  bound      : expr;
  step       : (kwd_step * expr) option;
  block      : block reg
}

and for_collect = {
  kwd_for    : kwd_for;
  var        : variable;
  bind_to    : (arrow * variable) option;
  kwd_in     : kwd_in;
  collection : collection;
  expr       : expr;
  block      : block reg
}

(* Code injection.  Note how the field [language] wraps a region in
   another: the outermost region covers the header "[%<language>" and
   the innermost covers the <language>. *)

and code_inj = {
  language : string reg reg;
  code     : expr;
  rbracket : rbracket;
}

and collection =
  Map  of kwd_map
| Set  of kwd_set
| List of kwd_list

(* Expressions *)

and expr =
  ECase    of expr case reg
| ECond    of cond_expr reg
| EAnnot   of annot_expr par reg
| ELogic   of logic_expr
| EArith   of arith_expr
| EString  of string_expr
| EList    of list_expr
| ESet     of set_expr
| EConstr  of (constr * arguments option) reg
| ERecord  of record reg
| EProj    of projection reg
| EModA    of expr module_access reg
| EUpdate  of update reg
| EMap     of map_expr
| EVar     of lexeme reg
| ECall    of fun_call
| EBytes   of (lexeme * Hex.t) reg
| ETuple   of tuple_expr
| EPar     of expr par reg
| EFun     of fun_expr reg
| ECodeInj of code_inj reg
| EBlock   of block_with reg

and annot_expr = expr * colon * type_expr

and set_expr =
  SetInj of expr injection reg
| SetMem of set_membership reg

and map_expr =
  MapLookUp of map_lookup reg
| MapInj    of binding reg injection reg
| BigMapInj of binding reg injection reg

and map_lookup = {
  path  : path;
  index : expr brackets reg
}

and path =
  Name of variable
| Path of projection reg

and logic_expr =
  BoolExpr of bool_expr
| CompExpr of comp_expr

and bool_expr =
  Or  of kwd_or  bin_op reg
| And of kwd_and bin_op reg
| Not of kwd_not un_op  reg

and 'a bin_op = {
  op   : 'a;
  arg1 : expr;
  arg2 : expr
}

and 'a un_op = {
  op  : 'a;
  arg : expr
}

and comp_expr =
  Lt    of lt    bin_op reg
| Leq   of leq   bin_op reg
| Gt    of gt    bin_op reg
| Geq   of geq   bin_op reg
| Equal of equal bin_op reg
| Neq   of neq   bin_op reg

and arith_expr =
  Add   of plus    bin_op reg
| Sub   of minus   bin_op reg
| Mult  of times   bin_op reg
| Div   of slash   bin_op reg
| Mod   of kwd_mod bin_op reg
| Neg   of minus    un_op reg
| Int   of (lexeme * Z.t) reg
| Nat   of (lexeme * Z.t) reg
| Mutez of (lexeme * Z.t) reg

and string_expr =
  Cat      of cat bin_op reg
| String   of lexeme reg
| Verbatim of lexeme reg

and list_expr =
  ECons     of cons bin_op reg
| EListComp of expr injection reg
| ENil      of kwd_nil

and field_assignment = {
  field_name : field_name;
  assignment : equal;
  field_expr : expr
}

and record = field_assignment reg ne_injection

and 'a module_access = {
  module_name : module_name;
  selector    : dot;
  field       : 'a;
}

and projection = {
  struct_name : variable;
  selector    : dot;
  field_path  : (selection, dot) nsepseq
}

and update = {
  record   : path;
  kwd_with : kwd_with;
  updates  : field_path_assignment reg ne_injection reg
}

and field_path_assignment = {
  field_path : path;
  assignment : equal;
  field_expr : expr
}

and selection =
  FieldName of field_name
| Component of (lexeme * Z.t) reg

and tuple_expr = (expr, comma) nsepseq par reg

and fun_call = (expr * arguments) reg

and arguments = tuple_expr

(* Injections *)

and 'a injection = {
  kind       : injection_kwd;
  enclosing  : enclosing;
  elements   : ('a, semi) sepseq;
  terminator : semi option
}

and injection_kwd =
  InjSet    of keyword
| InjMap    of keyword
| InjBigMap of keyword
| InjList   of keyword
| InjRecord of keyword

and enclosing =
  Brackets of lbracket * rbracket
| End      of kwd_end

and 'a ne_injection = {
  kind        : ne_injection_kwd;
  enclosing   : enclosing;
  ne_elements : ('a, semi) nsepseq;
  terminator  : semi option;
  attributes  : attributes
}

and ne_injection_kwd =
  NEInjSet    of keyword
| NEInjMap    of keyword
| NEInjRecord of keyword

(* Patterns *)

and pattern =
  PConstr of (constr * tuple_pattern option) reg
| PVar    of var_pattern reg
| PInt    of (lexeme * Z.t) reg
| PNat    of (lexeme * Z.t) reg
| PBytes  of (lexeme * Hex.t) reg
| PString of lexeme reg
| PList   of list_pattern
| PTuple  of tuple_pattern
| PRecord of field_pattern reg injection reg

and var_pattern = {
  variable   : variable;
  attributes : attribute list
}

and field_pattern = {
  field_name : field_name;
  eq         : equal;
  pattern    : pattern
}

and tuple_pattern = (pattern, comma) nsepseq par reg

and list_pattern =
  PListComp of pattern injection reg
| PNil      of kwd_nil
| PParCons  of (pattern * cons * pattern) par reg
| PCons     of (pattern, cons) nsepseq reg


(* PROJECTING REGIONS *)

let rec last to_region = function
    [] -> Region.ghost
|  [x] -> to_region x
| _::t -> last to_region t

let nsepseq_to_region to_region (hd,tl) =
  let reg (_, item) = to_region item in
  Region.cover (to_region hd) (last reg tl)

let type_expr_to_region = function
  TProd   {region; _}
| TSum    {region; _}
| TRecord {region; _}
| TApp    {region; _}
| TFun    {region; _}
| TPar    {region; _}
| TString {region; _}
| TInt    {region; _}
| TVar    {region; _}
| TModA   {region; _}
  -> region

let rec expr_to_region = function
  ELogic  e -> logic_expr_to_region e
| EArith  e -> arith_expr_to_region e
| EString e -> string_expr_to_region e
| EAnnot  e -> annot_expr_to_region e
| EList   e -> list_expr_to_region e
| ESet    e -> set_expr_to_region e
| ERecord e -> record_expr_to_region e
| EMap    e -> map_expr_to_region e
| ETuple  e -> tuple_expr_to_region e
| EConstr  {region; _}
| EUpdate  {region; _}
| EProj    {region; _}
| EModA    {region; _}
| EVar     {region; _}
| ECall    {region; _}
| EBytes   {region; _}
| ECase    {region;_}
| ECond    {region; _}
| EPar     {region; _}
| EFun     {region; _}
| ECodeInj {region; _}
| EBlock   {region; _} -> region

and tuple_expr_to_region {region; _} = region

and map_expr_to_region = function
  MapLookUp {region; _}
| MapInj    {region; _} -> region
| BigMapInj {region; _} -> region

and set_expr_to_region = function
  SetInj {region; _}
| SetMem {region; _} -> region

and logic_expr_to_region = function
  BoolExpr e -> bool_expr_to_region e
| CompExpr e -> comp_expr_to_region e

and bool_expr_to_region = function
  Or    {region; _}
| And   {region; _}
| Not   {region; _}
  -> region

and comp_expr_to_region = function
  Lt    {region; _}
| Leq   {region; _}
| Gt    {region; _}
| Geq   {region; _}
| Equal {region; _}
| Neq   {region; _} -> region

and arith_expr_to_region = function
  Add  {region; _}
| Sub  {region; _}
| Mult {region; _}
| Div  {region; _}
| Mod  {region; _}
| Neg  {region; _}
| Int  {region; _}
| Nat  {region; _}
| Mutez  {region; _} -> region

and string_expr_to_region = function
  Cat      {region; _}
| String   {region; _}
| Verbatim {region; _} -> region

and annot_expr_to_region {region; _} = region

and list_expr_to_region = function
  ECons {region; _}
| EListComp {region; _} -> region
| ENil t -> t#region

and record_expr_to_region {region; _} = region

let path_to_region = function
  Name var -> var.region
| Path {region; _} -> region

let instr_to_region = function
 Skip                t -> t#region
| Cond                {region; _}
| CaseInstr           {region; _}
| Assign              {region; _}
| Loop While          {region; _}
| Loop For ForInt     {region; _}
| Loop For ForCollect {region; _}
| ProcCall            {region; _}
| RecordPatch         {region; _}
| MapPatch            {region; _}
| SetPatch            {region; _}
| MapRemove           {region; _}
| SetRemove           {region; _} -> region

let clause_block_to_region = function
  LongBlock {region; _}
| ShortBlock {region; _} -> region

let if_clause_to_region = function
  ClauseInstr instr        -> instr_to_region instr
| ClauseBlock clause_block -> clause_block_to_region clause_block

let pattern_to_region = function
  PVar        {region; _}
| PInt        {region; _}
| PNat        {region; _}
| PBytes      {region; _}
| PString     {region; _}
| PConstr     {region; _}
| PList PListComp  {region; _}
| PList PParCons {region; _}
| PList PCons {region; _}
| PTuple      {region; _}
| PRecord     {region; _} -> region
| PList PNil t -> t#region

let declaration_to_region = function
  TypeDecl    {region;_}
| ConstDecl   {region;_}
| FunDecl     {region;_}
| ModuleDecl  {region;_}
| ModuleAlias {region;_} -> region
| Directive d -> Directive.to_region d

let lhs_to_region : lhs -> Region.t = function
  Path path -> path_to_region path
| MapPath {region; _} -> region

let selection_to_region = function
  FieldName {region; _}
| Component {region; _} -> region
