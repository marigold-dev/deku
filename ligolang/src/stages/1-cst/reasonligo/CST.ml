(* Concrete Syntax Tree (CST) for ReasonLIGO *)

(* To disable warning about multiply-defined record labels. *)

[@@@warning "-30-40-42"]

(* Vendor dependencies *)

module Directive = LexerLib.Directive
module Utils     = Simple_utils.Utils
module Region    = Simple_utils.Region
module Token     = Lexing_reasonligo.Token

open Utils
type 'a reg = 'a Region.reg

(* Lexemes *)

type lexeme = string

type 'payload wrap = 'payload Token.wrap 

(* Keywords of Reason *)

type kwd_and    = lexeme wrap
type kwd_else   = lexeme wrap
type kwd_false  = lexeme wrap
type kwd_fun    = lexeme wrap
type kwd_rec    = lexeme wrap
type kwd_if     = lexeme wrap
type kwd_let    = lexeme wrap
type kwd_switch = lexeme wrap
type kwd_mod    = lexeme wrap
type kwd_land   = lexeme wrap
type kwd_lor    = lexeme wrap
type kwd_lxor   = lexeme wrap
type kwd_lsl    = lexeme wrap
type kwd_lsr    = lexeme wrap
type kwd_not    = lexeme wrap
type kwd_of     = lexeme wrap
type kwd_or     = lexeme wrap
type kwd_then   = lexeme wrap
type kwd_true   = lexeme wrap
type kwd_type   = lexeme wrap
type kwd_module    = lexeme wrap

(* Data constructors *)

type c_None  = lexeme wrap
type c_Some  = lexeme wrap

(* Symbols *)

type arrow    = lexeme wrap  (* "=>"  *)
type cat      = lexeme wrap  (* "++"  *)
type dot      = lexeme wrap  (* "."   *)
type ellipsis = lexeme wrap  (* "..." *)
type equal    = lexeme wrap  (* "="   *)

(* Arithmetic operators *)

type minus    = lexeme wrap  (* "-" *)
type plus     = lexeme wrap  (* "+" *)
type slash    = lexeme wrap  (* "/" *)
type times    = lexeme wrap  (* "*" *)

(* Boolean operators *)

type bool_or  = lexeme wrap  (* "||" *)
type bool_and = lexeme wrap  (* "&&" *)

(* Comparisons *)

type equal_cmp = lexeme wrap  (* "=="  *)
type neq       = lexeme wrap  (* "!=" *)
type lt        = lexeme wrap  (* "<"  *)
type gt        = lexeme wrap  (* ">"  *)
type leq       = lexeme wrap  (* "<=" *)
type geq       = lexeme wrap  (* ">=" *)

(* Compounds *)

type lpar     = lexeme wrap  (* "(" *)
type rpar     = lexeme wrap  (* ")" *)
type lbracket = lexeme wrap  (* "[" *)
type rbracket = lexeme wrap  (* "]" *)
type lbrace   = lexeme wrap  (* "{" *)
type rbrace   = lexeme wrap  (* "}" *)

(* Separators *)

type comma = lexeme wrap  (* "," *)
type semi  = lexeme wrap  (* ";" *)
type vbar  = lexeme wrap  (* "|" *)
type colon = lexeme wrap  (* ":" *)

type quote = lexeme wrap  (* "'" *)

(* Wildcard *)

type wild = lexeme wrap  (* "_" *)

(* Virtual tokens *)

type eof = lexeme wrap

(* Literals *)

type variable    = string reg
type module_name = string reg
type fun_name    = string reg
type type_name   = string reg
type field_name  = string reg
type type_constr = string reg
type constr      = string reg
type attribute   = string reg

(* Parentheses *)

type 'a braces = {
  lbrace : lbrace;
  inside : 'a;
  rbrace : rbrace
}

type 'a par = {
  lpar   : lpar;
  inside : 'a;
  rpar   : rpar
}

type the_unit = lpar * rpar

(* The Abstract Syntax Tree *)

type t = {
  decl : declaration nseq;
  eof  : eof
}

and ast = t

and attributes = attribute list

and declaration =
  ConstDecl   of let_decl     reg
| TypeDecl    of type_decl    reg
| ModuleDecl  of module_decl  reg
| ModuleAlias of module_alias reg
| Directive   of Directive.t

(* Non-recursive values *)

and let_decl =
  (kwd_let * kwd_rec option * let_binding * attributes)

and let_binding = {
  binders  : pattern;
  lhs_type : (colon * type_expr) option;
  eq       : equal;
  let_rhs  : expr
}

(* Type declarations *)

and type_decl = {
  kwd_type   : kwd_type;
  name       : type_name;
  params     : type_vars option;
  eq         : equal;
  type_expr  : type_expr
}

and type_vars = (type_var reg, comma) nsepseq par reg

and type_var = {
  quote : quote;
  name  : variable
}

and module_decl = {
  kwd_module : kwd_module;
  name       : module_name;
  eq         : equal;
  lbrace     : lbrace;
  module_    : t;
  rbrace     : rbrace;
}

and module_alias = {
  kwd_module : kwd_module;
  alias      : module_name;
  eq         : equal;
  binders    : (module_name, dot) nsepseq;
}

and type_expr =
  TProd   of cartesian
| TSum    of sum_type reg
| TRecord of field_decl reg ne_injection reg
| TApp    of (type_constr * type_args) reg
| TFun    of (type_expr * arrow * type_expr) reg
| TPar    of type_expr par reg
| TVar    of variable
| TString of lexeme reg
| TInt    of (lexeme * Z.t) reg
| TModA   of type_expr module_access reg
| TArg    of type_var reg

and cartesian = (type_expr, comma) nsepseq par reg

and sum_type = {
  lead_vbar  : vbar option;
  variants   : (variant reg, vbar) nsepseq;
  attributes : attributes
}

and variant = {
  constr     : constr;
  args       : type_expr par reg option;
  attributes : attributes
}

and field_decl = {
  field_name : field_name;
  colon      : colon;
  field_type : type_expr;
  attributes : attributes
}

and type_args = (type_expr, comma) nsepseq par reg

and pattern =
  PConstr   of (constr * pattern option) reg
| PUnit     of the_unit reg
| PVar      of var_pattern reg
| PInt      of (lexeme * Z.t) reg
| PNat      of (lexeme * Z.t) reg
| PBytes    of (lexeme * Hex.t) reg
| PString   of string reg
| PVerbatim of string reg
| PList     of list_pattern
| PTuple    of (pattern, comma) nsepseq reg
| PPar      of pattern par reg
| PRecord   of field_pattern reg ne_injection reg
| PTyped    of typed_pattern reg

and var_pattern = {
  variable   : variable;
  attributes : attribute list
}

and list_pattern =
  PListComp of pattern injection reg
| PCons     of cons_pattern reg

and cons_pattern = {
  lbracket : lbracket;
  lpattern : pattern;
  comma    : comma;
  ellipsis : ellipsis;
  rpattern : pattern;
  rbracket : rbracket;
}

and typed_pattern = {
  pattern   : pattern;
  colon     : colon;
  type_expr : type_expr
}

and field_pattern = {
  field_name : field_name;
  eq         : equal;
  pattern    : pattern
}

and expr =
  ECase     of expr case reg
| ECond     of cond_expr reg
| EAnnot    of annot_expr reg
| ELogic    of logic_expr
| EArith    of arith_expr
| EString   of string_expr
| EList     of list_expr
| EConstr   of (constr * expr option) reg
| ERecord   of record reg
| EProj     of projection reg
| EModA     of expr module_access reg
| EUpdate   of update reg
| EVar      of variable
| ECall     of (expr * arguments) reg
| EBytes    of (string * Hex.t) reg
| EUnit     of the_unit reg
| ETuple    of (expr, comma) nsepseq reg
| EPar      of expr par reg
| ELetIn    of let_in reg
| ETypeIn   of type_in reg
| EModIn    of mod_in reg
| EModAlias of mod_alias reg
| EFun      of fun_expr reg
| ESeq      of expr injection reg
| ECodeInj  of code_inj reg

and arguments =
  Multiple of (expr,comma) nsepseq par reg
| Unit     of the_unit reg

and annot_expr = expr * colon * type_expr

and 'a injection = {
  compound   : compound option;
  elements   : ('a, semi) sepseq;
  terminator : semi option
}

and 'a ne_injection = {
  compound    : compound option;
  ne_elements : ('a, semi) nsepseq;
  terminator  : semi option;
  attributes  : attributes
}

and compound = [
  `Braces   of lbrace * rbrace
| `Brackets of lbracket * rbracket]

and list_expr =
  ECons     of cons_expr reg
| EListComp of expr injection reg

and cons_expr =
  { lbracket : lbracket;
    lexpr    : expr;
    comma    : comma;
    ellipsis : ellipsis;
    rexpr    : expr;
    rbracket : rbracket;
  }

and string_expr =
  Cat      of cat bin_op reg
| String   of string reg
| Verbatim of string reg

and arith_expr =
  Add   of plus bin_op reg
| Sub   of minus bin_op reg
| Mult  of times bin_op reg
| Div   of slash bin_op reg
| Mod   of kwd_mod bin_op reg
| Land  of kwd_land bin_op reg
| Lor   of kwd_lor bin_op reg
| Lxor  of kwd_lxor bin_op reg
| Lsl   of kwd_lsl bin_op reg
| Lsr   of kwd_lsr bin_op reg
| Neg   of minus un_op reg
| Int   of (string * Z.t) reg
| Nat   of (string * Z.t) reg
| Mutez of (string * Z.t) reg

and logic_expr =
  BoolExpr of bool_expr
| CompExpr of comp_expr

and bool_expr =
  Or  of kwd_or bin_op reg
| And of kwd_and bin_op reg
| Not of kwd_not un_op reg

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
  Lt    of lt        bin_op reg
| Leq   of leq       bin_op reg
| Gt    of gt        bin_op reg
| Geq   of geq       bin_op reg
| Equal of equal_cmp bin_op reg
| Neq   of neq       bin_op reg

and record = field_assign reg ne_injection

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

and selection =
  FieldName of variable
| Component of (string * Z.t) reg

and field_assign = {
  field_name : field_name;
  assignment : colon;
  field_expr : expr
}

and update = {
  lbrace   : lbrace;
  ellipsis : ellipsis;
  record   : path;
  comma    : comma;
  updates  : field_path_assignment reg ne_injection reg;
  rbrace   : rbrace
}

and field_path_assignment = {
  field_path : path;
  assignment : colon;
  field_expr : expr
}

and path =
  Name of variable
| Path of projection reg

and 'a case = {
  kwd_switch : kwd_switch;
  expr       : expr;
  lbrace     : lbrace;
  cases      : ('a case_clause reg, vbar) nsepseq reg;
  rbrace     : rbrace
}

and 'a case_clause = {
  pattern    : pattern;
  arrow      : arrow;
  rhs        : 'a;
  terminator : comma option
}

and let_in = {
  kwd_let    : kwd_let;
  kwd_rec    : kwd_rec option;
  binding    : let_binding;
  semi       : semi;
  body       : expr;
  attributes : attributes
}

and type_in = {
  type_decl : type_decl;
  semi      : semi;
  body      : expr;
}

and mod_in = {
  mod_decl : module_decl;
  semi     : semi;
  body     : expr;
}

and mod_alias = {
  mod_alias : module_alias;
  semi      : semi;
  body      : expr;
}

and fun_expr = {
  binders     : pattern;
  lhs_type    : (colon * type_expr) option;
  arrow       : arrow;
  body        : expr;
  attributes  : attributes
}

and cond_expr = {
  kwd_if   : kwd_if;
  test     : test_expr;
  ifso     : branch;
  ifnot    : (kwd_else * branch) option;
}

and test_expr = [
  `Braces of expr braces reg
| `Parens of expr par reg]

and branch = (expr * semi option) braces reg

(* Code injection.  Note how the field [language] wraps a region in
   another: the outermost region covers the header "[%<language>" and
   the innermost covers the <language>. *)

and code_inj = {
  language : string reg reg;
  code     : expr;
  rbracket : rbracket;
}

(* Projecting regions from some nodes of the AST *)

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
| TArg    {region; _}
 -> region

let list_pattern_to_region = function
  PListComp {region; _} | PCons {region; _} -> region

let pattern_to_region = function
| PList p -> list_pattern_to_region p
| PConstr {region;_}
| PUnit {region;_}
| PTuple {region;_} | PVar {region;_}
| PInt {region;_}
| PString {region;_} | PVerbatim {region;_}
| PPar {region;_}
| PRecord {region; _} | PTyped {region; _}
| PNat {region; _} | PBytes {region; _}
  -> region

let bool_expr_to_region = function
  Or {region;_} | And {region;_}
| Not {region;_} -> region

let comp_expr_to_region = function
  Lt {region;_} | Leq {region;_}
| Gt {region;_} | Geq {region;_}
| Neq {region;_} | Equal {region;_} -> region

let logic_expr_to_region = function
  BoolExpr e -> bool_expr_to_region e
| CompExpr e -> comp_expr_to_region e

let arith_expr_to_region = function
  Add {region;_} | Sub {region;_} | Mult {region;_}
| Div {region;_} | Mod {region;_} | Neg {region;_}
| Land {region;_} | Lor {region;_} | Lxor {region;_} 
| Lsl {region;_} | Lsr {region;_}
| Int {region;_} | Mutez {region; _}
| Nat {region; _} -> region

let string_expr_to_region = function
  Verbatim {region;_} | String {region;_} | Cat {region;_} -> region

let list_expr_to_region = function
  ECons {region; _} | EListComp {region; _}
(* | Append {region; _}*) -> region

let expr_to_region = function
  ELogic e -> logic_expr_to_region e
| EArith e -> arith_expr_to_region e
| EString e -> string_expr_to_region e
| EList e -> list_expr_to_region e
| EConstr {region;_}
| EAnnot {region;_ } | ELetIn {region;_}   | EFun {region;_}
| ETypeIn {region;_ }| EModIn {region;_}   | EModAlias {region;_}
| ECond {region;_}   | ETuple {region;_}   | ECase {region;_}
| ECall {region;_}   | EVar {region; _}    | EProj {region; _}
| EUnit {region;_}   | EPar {region;_}     | EBytes {region; _}
| ESeq {region; _}   | ERecord {region; _} | EUpdate {region; _}
| EModA {region; _}
| ECodeInj {region; _} -> region

let declaration_to_region = function
  ConstDecl   {region;_}
| TypeDecl    {region;_}
| ModuleDecl  {region;_}
| ModuleAlias {region;_} -> region
| Directive d -> Directive.to_region d

let selection_to_region = function
  FieldName f -> f.region
| Component c -> c.region

let path_to_region = function
  Name var -> var.region
| Path {region; _} -> region
