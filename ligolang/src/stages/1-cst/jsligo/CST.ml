(* Concrete Syntax Tree (CST) for JsLIGO *)

(* To disable warning about multiply-defined record labels. *)

[@@@warning "-30-40-42"]

(* Vendor dependencies *)

module Directive = LexerLib.Directive
module Utils     = Simple_utils.Utils
module Region    = Simple_utils.Region
module Token     = Lexing_jsligo.Token

open Utils
type 'a reg = 'a Region.reg

(* Lexemes *)

type lexeme       = string

type 'payload wrap = 'payload Token.wrap 

(* Keywords of JsLIGO *)

type kwd_else      = lexeme wrap
type kwd_if        = lexeme wrap
type kwd_let       = lexeme wrap
type kwd_const     = lexeme wrap
type kwd_or        = lexeme wrap
type kwd_then      = lexeme wrap
type kwd_type      = lexeme wrap
type kwd_return    = lexeme wrap
type kwd_switch    = lexeme wrap
type kwd_case      = lexeme wrap
type kwd_default   = lexeme wrap
type kwd_as        = lexeme wrap
type kwd_break     = lexeme wrap
type kwd_namespace = lexeme wrap
type kwd_export    = lexeme wrap
type kwd_import    = lexeme wrap
type kwd_while     = lexeme wrap
type kwd_for       = lexeme wrap
type kwd_of        = lexeme wrap

(* Symbols *)

type arrow    = lexeme wrap  (* "=>"  *)
type dot      = lexeme wrap  (* "."   *)
type ellipsis = lexeme wrap  (* "..." *)
type equal    = lexeme wrap  (* "="   *)

(* Arithmetic operators *)

type minus      = lexeme wrap  (* "-" *)
type plus       = lexeme wrap  (* "+" *)
type slash      = lexeme wrap  (* "/" *)
type modulo     = lexeme wrap  (* "%" *)
type times      = lexeme wrap  (* "*" *)

(* Boolean operators *)

type bool_or  = lexeme wrap  (* "||" *)
type bool_and = lexeme wrap  (* "&&" *)

(* Comparisons *)

type negate    = lexeme wrap  (* "!"  *)
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

(* Wildcard *)

type wild = lexeme wrap  (* "_" *)

(* Virtual tokens *)

type eof = lexeme wrap

(* Literals *)

type variable     = string reg
type fun_name     = string reg
type type_name    = string reg
type type_var     = string reg
type type_constr  = string reg
type constr       = string reg
type attribute    = string reg
type field_name   = string reg
type module_name  = string reg


(* Parentheses *)

type 'a braces = {
  lbrace : lbrace;
  inside : 'a;
  rbrace : rbrace
}

type 'a brackets = {
  lbracket : lbracket;
  inside   : 'a;
  rbracket : rbracket
}

type 'a chevrons = {
  lchevron   : lt;
  inside     : 'a;
  rchevron   : gt
}

type 'a par = {
  lpar   : lpar;
  inside : 'a;
  rpar   : rpar
}

type the_unit = lpar * rpar

(* The Abstract Syntax Tree *)

type t = {
  statements : toplevel_statements;
  eof        : eof
}

and toplevel_statements = toplevel_statement nseq

and toplevel_statement =
  TopLevel  of statement * semi option
| Directive of Directive.t

and ast = t

and attributes = attribute list

(* Non-recursive values *)

and val_binding = {
  binders    : pattern;
  lhs_type   : (colon * type_expr) option;
  eq         : equal;
  expr       : expr
}

(* Type declarations *)

and type_decl = {
  attributes : attributes;
  kwd_type   : kwd_type;
  name       : type_name;
  params     : type_vars option;
  eq         : equal;
  type_expr  : type_expr
}

and type_vars = (type_var, comma) nsepseq chevrons reg

and fun_type_args = (fun_type_arg, comma) nsepseq par

and fun_type_arg = {
  name      : variable;
  colon     : colon;
  type_expr : type_expr
}

and type_expr =
  TProd   of cartesian
| TSum    of sum_type reg
| TObject of field_decl reg ne_injection reg
| TApp    of (type_constr * type_params) reg
| TFun    of (fun_type_args * arrow * type_expr) reg
| TPar    of type_expr par reg
| TVar    of variable
| TString of lexeme reg
| TInt    of (lexeme * Z.t) reg
| TModA   of type_expr module_access reg

and type_params = (type_expr, comma) nsepseq chevrons reg

and cartesian = {
  inside     : (type_expr, comma) nsepseq brackets reg;
  attributes : attributes
}

and 'a module_access = {
  module_name : module_name;
  selector    : dot;
  field       : 'a;
}

and sum_type = {
  leading_vbar : vbar option;
  variants     : (variant reg, vbar) nsepseq reg;
  attributes   : attributes
}

and variant = {
  tuple        : variant_comp brackets reg;
  attributes   : attributes
}

and variant_comp = {
  constr : constr;
  params : (comma * (type_expr, comma) nsepseq) option
}

and field_decl = {
  field_name : field_name;
  colon      : colon;
  field_type : type_expr;
  attributes : attributes
}

and rest_pattern = {
  ellipsis  : ellipsis;
  rest      : variable
}

and assign_pattern = {
  property  : variable;
  eq        : equal;
  value     : expr
}

and destruct = {
  property  : variable;
  colon     : colon;
  target    : val_binding reg;
}

and pattern =
  PRest     of rest_pattern reg
| PAssign   of assign_pattern reg
| PVar      of var_pattern reg
| PConstr   of variable
| PDestruct of destruct reg
| PObject   of object_pattern
| PArray    of array_pattern

and array_pattern = (pattern, comma) nsepseq brackets reg

and object_pattern = (pattern, comma) nsepseq braces reg

and var_pattern = {
  variable   : variable;
  attributes : attribute list
}

and string_expr =
  String   of string reg
| Verbatim of string reg

and return = {
  kwd_return: kwd_return;
  expr: expr option;
}

and switch = {
  kwd_switch  : kwd_switch;
  lpar        : lpar;
  expr        : expr;
  rpar        : rpar;
  lbrace      : lbrace;
  cases       : switch_case nseq;
  rbrace      : rbrace;
}

and switch_case =
  Switch_case of {
    kwd_case    : kwd_case;
    expr        : expr;
    colon       : colon;
    statements  : statements option;
  }
| Switch_default_case of {
    kwd_default : kwd_default;
    colon       : colon;
    statements  : statements option;
  }

and array_item_rest = {
  ellipsis : ellipsis;
  expr     : expr
}

and array_item =
  Expr_entry of expr
| Rest_entry of array_item_rest reg

and property2 = {
  name  : expr;
  colon : colon;
  value : expr
}

and property_rest = {
  ellipsis : ellipsis;
  expr     : expr
}

and property =
  Punned_property of expr reg
| Property        of property2 reg
| Property_rest   of property_rest reg

and expr =
  EFun     of fun_expr reg
| EPar     of expr par reg
| ESeq     of (expr, comma) nsepseq reg
| EVar     of variable
| EModA    of expr module_access reg
| ELogic   of logic_expr
| EArith   of arith_expr
| ECall    of (expr * arguments) reg
| EBytes   of (string * Hex.t) reg
| EArray   of (array_item, comma) sepseq brackets reg
| EObject  of object_expr
| EString  of string_expr
| EProj    of projection reg
| EAssign  of expr * operator reg * expr
| EConstr  of (constr * expr option) reg
| EAnnot   of annot_expr reg
| EUnit    of the_unit reg
| ECodeInj of code_inj reg

and assignment_operator = 
  Times_eq
| Div_eq
| Min_eq 
| Plus_eq
| Mod_eq

and operator =
  Eq
| Assignment_operator of assignment_operator

and object_expr = (property, comma) nsepseq braces reg

and statement =
  SBlock      of (statement, semi) nsepseq braces reg
| SExpr       of expr
| SCond       of cond_statement reg
| SReturn     of return reg
| SLet        of let_decl reg
| SConst      of const_decl reg
| SType       of type_decl reg
| SSwitch     of switch reg
| SBreak      of kwd_break
| SNamespace  of namespace_statement
| SExport     of (kwd_export * statement) reg
| SImport     of import reg
| SWhile      of while_stmt reg
| SForOf      of for_of reg

and namespace_statement =
  (kwd_namespace * module_name * statements braces reg * attributes) reg

and while_stmt = {
  kwd_while: kwd_while;
  lpar:      lpar;
  expr:      expr;
  rpar:      rpar;
  statement: statement;
}

and for_of = {
  kwd_for    : kwd_for;
  lpar       : lpar;
  index_kind : index_kind;
  index      : variable;
  kwd_of     : kwd_of;
  expr       : expr;
  rpar       : rpar;
  statement  : statement
}

and index_kind = [
  `Let   of kwd_let
| `Const of kwd_const]

and import = {
  kwd_import   : kwd_import;
  alias        : module_name;
  equal        : equal;
  module_path  : (module_name, dot) nsepseq
}

and statements = (statement, semi) nsepseq

and arguments =
  Multiple of (expr, comma) nsepseq par reg
| Unit     of the_unit reg

and annot_expr = expr * kwd_as * type_expr (* TODO: Used with colon too in Parser *)

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

and compound =
  Braces   of lbrace * rbrace
| Brackets of lbracket * rbracket

and arith_expr =
  Add   of plus bin_op reg
| Sub   of minus bin_op reg
| Mult  of times bin_op reg
| Div   of slash bin_op reg
| Mod   of modulo bin_op reg
| Neg   of minus un_op reg
| Int   of (string * Z.t) reg

and logic_expr =
  BoolExpr of bool_expr
| CompExpr of comp_expr

and bool_expr =
  Or   of bool_or bin_op reg
| And  of bool_and bin_op reg
| Not  of negate un_op reg

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

and projection = {
  expr      : expr;
  selection : selection;
}

and selection_field_name = {
  dot   : dot;
  value : variable
}

and selection =
  FieldName of selection_field_name reg
| Component of expr brackets reg

and let_decl = {
  attributes : attributes;
  kwd_let    : kwd_let;
  bindings   : (val_binding reg, comma) nsepseq
}

and const_decl = {
  attributes : attributes;
  kwd_const  : kwd_const;
  bindings   : (val_binding reg, comma) nsepseq;
}

and body =
  FunctionBody   of statements braces reg
| ExpressionBody of expr

and fun_expr = {
  parameters : expr;
  lhs_type   : (colon * type_expr) option;
  arrow      : arrow;
  body       : body;
}

and cond_statement = {
  kwd_if : kwd_if;
  test   : expr par;
  ifso   : statement;
  ifnot  : (kwd_else * statement) option;
}

(* Code injection.  Note how the field [language] wraps a region in
   another: the outermost region covers the header "[%<language>" and
   the innermost covers the <language>. *)

and code_inj = {
  language : string reg;
  code     : expr;
}

(* Projecting regions from some nodes of the AST *)

let rec last to_region = function
    [] -> Region.ghost
|  [x] -> to_region x
| _::t -> last to_region t

let nseq_to_region to_region (hd,tl) =
  Region.cover (to_region hd) (last to_region tl)

let nsepseq_to_region to_region (hd,tl) =
  let reg (_, item) = to_region item in
  Region.cover (to_region hd) (last reg tl)

let type_expr_to_region = function
  TProd   {inside = {region; _}; _}
| TSum    {region; _}
| TObject {region; _}
| TApp    {region; _}
| TFun    {region; _}
| TPar    {region; _}
| TString {region; _}
| TVar    {region; _}
| TModA   {region; _}
| TInt    {region; _}
 -> region

let pattern_to_region = function
  PRest {region;_ }   | PAssign {region ;_ }
| PVar {region ;_ }    | PConstr {region; _ } | PDestruct {region ;_ }
| PObject {region ;_ } | PArray {region; _} -> region

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
| Int {region;_} -> region

let string_expr_to_region = function
  Verbatim {region;_} | String {region;_} -> region

let rec expr_to_region = function
  ELogic e -> logic_expr_to_region e
| EArith e -> arith_expr_to_region e
| EString e -> string_expr_to_region e
| EAssign (f, _, e) ->
    Region.cover (expr_to_region f) (expr_to_region e)
| EConstr {region; _}
| EAnnot {region;_ } | EFun {region;_}
| ECall {region;_}   | EVar {region; _}    | EProj {region; _}
| EUnit {region;_}   | EPar {region;_}     | EBytes {region; _}
| ESeq {region; _}   | EObject {region; _} | EArray { region; _}
| ECodeInj {region; _} | EModA { region; _} -> region

let statement_to_region = function
  SBreak b -> b#region
| SExpr e -> expr_to_region e
| SBlock {region; _ }
| SCond {region; _}
| SReturn {region; _}
| SLet  {region; _}
| SConst {region; _}
| SSwitch {region; _}
| SType {region; _}
| SImport {region; _}
| SExport {region; _}
| SForOf {region; _}
| SWhile {region; _}
| SNamespace {region; _} -> region

let selection_to_region = function
  FieldName f -> f.region
| Component c -> c.region

let body_to_region = function
  FunctionBody {region; _} -> region
| ExpressionBody s -> expr_to_region s

let property_to_region = function
  Punned_property {region; _}
| Property {region; _}
| Property_rest {region; _} -> region

let array_item_to_region = function
  Expr_entry e -> expr_to_region e
| Rest_entry {region; _} -> region
