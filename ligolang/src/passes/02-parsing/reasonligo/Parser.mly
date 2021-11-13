%{
(* START HEADER *)

[@@@warning "-42"]

(* Vendors dependencies *)

open Simple_utils.Region

(* LIGO dependencies *)

module CST = Cst_reasonligo.CST
open! CST
module Wrap = Lexing_shared.Wrap

(* Utilities *)

let unwrap = Wrap.payload
let wrap   = Wrap.wrap

let ghost = wrap "" ghost

let mk_wild region =
  let variable = {value="_"; region} in
  let value = {variable; attributes=[]}
  in {region; value}

let list_of_option = function
       None -> []
| Some list -> list

(* END HEADER *)
%}

(* Reductions on error *)
(* TODO *)

(* See [ParToken.mly] for the definition of tokens. *)

(* Entry points *)

%start contract interactive_expr
%type <CST.t> contract
%type <CST.expr> interactive_expr

%%

(* RULES *)

(* Compound constructs *)

par(X):
  "(" X ")" {
    let lpar = $1 in
    let rpar = $3 in
    let region = cover lpar#region rpar#region
    and value  = {lpar; inside=$2; rpar}
    in {region; value} }

brackets(X):
  "[" X "]" {
    let lbracket = $1 in
    let rbracket = $3 in
    let region = cover lbracket#region rbracket#region
    and value  = {lbracket; inside=$2; rbracket}
    in {region; value} }

braces(X):
  "{" X "}" {
    let lbrace = $1 in
    let rbrace = $3 in
    let region = cover lbrace#region rbrace#region
    and value  = {lbrace; inside=$2; rbrace}
    in {region; value} }

(* Sequences

   Series of instances of the same syntactical category have often to
   be parsed, like lists of expressions, patterns etc. The simplest of
   all is the possibly empty sequence (series), parsed below by
   [seq]. The non-empty sequence is parsed by [nseq]. Note that the
   latter returns a pair made of the first parsed item (the parameter
   [X]) and the rest of the sequence (possibly empty). This way, the
   OCaml typechecker can keep track of this information along the
   static control-flow graph. The rule [sepseq] parses possibly empty
   sequences of items separated by some token (e.g., a comma), and
   rule [nsepseq] is for non-empty such sequences. See module [Utils]
   for the types corresponding to the semantic actions of those
   rules. *)

(* Possibly empty sequence of items *)

seq(item):
  (**)           {     [] }
| item seq(item) { $1::$2 }

(* Non-empty sequence of items *)

nseq(item):
  item seq(item) { $1,$2 }

(* Non-empty separated sequence of items *)

nsepseq(item,sep):
  item                       {                        $1, [] }
| item sep nsepseq(item,sep) { let h,t = $3 in $1, ($2,h)::t }

(* The rule [sep_or_term(item,sep)] ("separated or terminated list")
   parses a non-empty list of items separated by [sep], and optionally
   terminated by [sep]. *)

sep_or_term_list(item,sep):
  nsepseq(item,sep) {
    $1, None
  }
| nseq(item sep {$1,$2}) {
    let (first,sep), tail = $1 in
    let rec trans (seq, prev_sep as acc) = function
      [] -> acc
    | (item,next_sep)::others ->
        trans ((prev_sep,item)::seq, next_sep) others in
    let list, term = trans ([],sep) tail
    in (first, List.rev list), Some term }

(* Helpers *)

%inline variable         : "<ident>"  { unwrap $1 }
%inline type_name        : "<ident>"  { unwrap $1 }
%inline field_name       : "<ident>"  { unwrap $1 }
%inline struct_name      : "<ident>"  { unwrap $1 }
%inline module_name      : "<uident>" { unwrap $1 }

(* Non-empty comma-separated values (at least two values) *)

tuple(item):
  item "," nsepseq(item,",") { let h,t = $3 in $1, ($2,h)::t }

(* Possibly empty semicolon-separated values between brackets *)

list_of(item):
  "[" sep_or_term_list(item,",")? "]" {
    let lbracket = $1 in
    let rbracket = $3 in
    let compound = Some (`Brackets (lbracket,rbracket))
    and region = cover lbracket#region rbracket#region in
    let elements, terminator =
      match $2 with
        None -> None, None
      | Some (elements, terminator) ->
          Some elements, terminator in
    let value = {compound; elements; terminator}
    in {region; value} }

(* Main *)

contract:
  nseq(declaration) EOF { {decl=$1; eof=$2} }

declaration:
  type_decl ";"?        { TypeDecl    $1 }
| let_declaration ";"?  { ConstDecl   $1 }
| module_decl ";"?      { ModuleDecl  $1 }
| module_alias ";"?     { ModuleAlias $1 }
| "<directive>"         { Directive   $1 }

module_:
  nseq(declaration) { {decl=$1; eof=ghost} }

(* Attributes *)

%inline attributes:
  ioption(nseq("[@attr]") { Utils.nseq_to_list $1 }) {
    let l = list_of_option $1 in
    List.map unwrap l
  }

(* Type declarations *)

type_decl:
  "type" type_name type_params? "=" type_expr {
    let kwd_type = $1 in
    let eq = $4 in
    let region = cover kwd_type#region (type_expr_to_region $5) in
    let value  = {kwd_type;
                  name       = $2;
                  params     = $3;
                  eq;
                  type_expr  = $5}
    in {region; value} }

type_params:
  par(nsepseq(type_var,",")) { $1 }

type_var:
  "'" variable {
    let quote = $1 in
    let region = cover quote#region $2.region
    and value = {quote; name=$2}
    in {region; value} }

module_decl:
  "module" module_name "=" "{" module_ "}" {
    let kwd_module = $1 in
    let eq = $3 in
    let lbrace = $4 in
    let rbrace = $6 in
    let region = cover kwd_module#region rbrace#region in
    let value  = {kwd_module;
                  name       = $2;
                  eq;
                  lbrace;
                  module_    = $5;
                  rbrace}
    in {region; value} }

module_alias:
  "module" module_name "=" nsepseq (module_name,".") {
    let kwd_module = $1 in
    let eq = $3 in
    let stop   = nsepseq_to_region (fun x -> x.region) $4 in
    let region = cover kwd_module#region stop in
    let value  = {kwd_module;
                  alias      = $2;
                  eq;
                  binders    = $4}
    in {region; value} }

type_expr:
  fun_type_level | sum_type { $1 }

fun_type_level:
  ES6FUN core_type "=>" fun_type_level {
    let start  = type_expr_to_region $2
    and stop   = type_expr_to_region $4 in
    let region = cover start stop in
    TFun {region; value=$2,$3,$4}
  }
| core_type { $1 }

core_type:
  "<string>"            { TString (unwrap $1) }
| "<int>"               { TInt    (unwrap $1) }
| "_"                   { TVar {value="_"; region=$1#region } }
| type_name             { TVar    $1 }
| module_access_t       { TModA   $1 }
| type_constr_app       { TApp    $1 }
| record_type           { TRecord $1 }
| type_var              { TArg    $1 }
| par(tuple(type_expr)) { TProd   $1 }
| par(type_expr)        { TPar    $1 }

type_constr_app:
  type_name par(type_args) {
    let region = cover $1.region $2.region
    in {region; value = $1,$2} }

type_args:
  type_expr        { $1, [] }
| tuple(type_expr) { $1 }

sum_type:
  nsepseq(variant,"|") {
    let region = nsepseq_to_region (fun x -> x.region) $1 in
    let value  = {variants=$1; attributes=[]; lead_vbar=None}
    in TSum {region; value}
  }
| attributes "|" nsepseq(variant,"|") {
    let region = nsepseq_to_region (fun x -> x.region) $3 in
    let value  = {variants=$3; attributes=$1; lead_vbar = Some $2}
    in TSum {region; value} }

(* Always use [ioption] at the end of a rule *)

variant:
  attributes "<uident>" ioption(par(type_expr)) {
    let constr = unwrap $2 in
    let stop   = match $3 with
                   None -> constr.region
                 | Some t -> t.region in
    let region = cover constr.region stop
    and value = {constr=constr; args=$3; attributes=$1}
    in {region; value} }

record_type:
  attributes "{" sep_or_term_list(field_decl,",") "}" {
    let lbrace = $2 in
    let rbrace = $4 in
    let fields, terminator = $3 in
    let region = cover lbrace#region rbrace#region
    and value = {
      compound = Some (`Braces (lbrace,rbrace));
      ne_elements = fields;
      terminator;
      attributes=$1}
    in {region; value} }

module_access_t :
  module_name "." module_var_t {
    let start  = $1.region in
    let stop   = type_expr_to_region $3 in
    let region = cover start stop in
    let value  = {module_name=$1; selector=$2; field=$3}
    in {region; value} }

module_var_t:
  module_access_t   { TModA $1 }
| field_name        { TVar  $1 }

field_decl:
  attributes field_name {
    let value = {
      field_name=$2;
      colon=ghost;
      field_type = TVar $2;
      attributes=$1}
    in {$2 with value}
  }
| attributes field_name type_annotation {
    let colon, field_type = $3 in
    let stop   = type_expr_to_region field_type in
    let region = cover $2.region stop
    and value  = {field_name=$2; colon; field_type; attributes=$1}
    in {region; value} }

(* Top-level definitions *)

let_declaration:
  attributes "let" ioption("rec") let_binding {
    let kwd_rec = $3 in
    let value  = $2, kwd_rec, $4, $1 in
    let stop   = expr_to_region $4.let_rhs in
    let region = cover $2#region stop
    in {region; value} }

let_binding:
  irrefutable type_annotation? "=" expr {
    {binders = $1; lhs_type=$2; eq=$3; let_rhs=$4} }

type_annotation:
  ":" type_expr { $1,$2 }

(* Patterns *)

irrefutable:
  sub_irrefutable        { $1 }
| tuple(sub_irrefutable) {
    let region = nsepseq_to_region pattern_to_region $1
    in PTuple {region; value=$1} }

sub_irrefutable:
  "_"                     { PVar    (mk_wild $1#region) }
| var_pattern             { PVar    $1 }
| unit                    { PUnit   $1 }
| record_pattern          { PRecord $1 }
| par(closed_irrefutable) { PPar    $1 }

closed_irrefutable:
  irrefutable     {         $1 }
| constr_pattern  { PConstr $1 }
| typed_pattern   {  PTyped $1 }

var_pattern:
  attributes "<ident>" {
    let variable = unwrap $2 in
    let value = {variable; attributes=$1}
    in {variable with value} }

typed_pattern:
  irrefutable ":" type_expr  {
    let start  = pattern_to_region $1 in
    let stop   = type_expr_to_region $3 in
    let region = cover start stop in
    let value  = {pattern=$1; colon=$2; type_expr=$3}
    in {region; value} }

pattern:
  core_pattern { $1 }
| "[" sub_pattern "," "..." sub_pattern "]" {
    let lbracket = $1 in
    let comma = $3 in
    let ellipsis = $4 in
    let rbracket = $6 in
    let start  = pattern_to_region $2 in
    let stop   = pattern_to_region $5 in
    let region = cover start stop in
    let value  =
      { lbracket;
        lpattern = $2;
        comma;
        ellipsis;
        rpattern = $5;
        rbracket}
    in PList (PCons {value;region})
  }
| tuple(sub_pattern) {
    let region = nsepseq_to_region pattern_to_region $1
    in PTuple {region; value=$1} }

sub_pattern:
  par(sub_pattern) { PPar $1 }
| core_pattern     {      $1 }

core_pattern:
  "<int>"              { PInt      (unwrap $1) }
| "<nat>"              { PNat      (unwrap $1) }
| "<bytes>"            { PBytes    (unwrap $1) }
| "<string>"           { PString   (unwrap $1) }
| "<verbatim>"         { PVerbatim (unwrap $1) }
| "_"                  { PVar      (mk_wild $1#region) }
| var_pattern          { PVar      $1 }
| unit                 { PUnit     $1 }
| par(ptuple)          { PPar      $1 }
| list_of(sub_pattern) { PList     (PListComp $1) }
| constr_pattern       { PConstr   $1 }
| record_pattern       { PRecord   $1 }

record_pattern:
  "{" sep_or_term_list(field_pattern,",") "}" {
    let lbrace = $1 in
    let rbrace = $3 in
    let ne_elements, terminator = $2 in
    let region = cover lbrace#region rbrace#region in
    let value  = {
      compound = Some (`Braces (lbrace,rbrace));
      ne_elements;
      terminator;
      attributes=[]}
    in {region; value} }

field_pattern:
  field_name {
    let region  = $1.region in
    let pattern = PVar {region; value = {variable=$1; attributes=[]}} in
    let value   = {field_name=$1; eq=ghost; pattern}
    in {region; value}
  }
| field_name ":" sub_pattern {
    let start  = $1.region
    and stop   = pattern_to_region $3 in
    let region = cover start stop
    and value  = {field_name=$1; eq=$2; pattern=$3}
    in {region; value} }

constr_pattern:
  "<uident>" ioption(sub_pattern) {
    let constr = unwrap $1 in
    let region =
      match $2 with
        None -> constr.region
      | Some stop -> cover constr.region (pattern_to_region stop)
    in {region; value = (constr,$2)} }

ptuple:
  tuple(sub_pattern) {
    let region = nsepseq_to_region pattern_to_region $1
    in PTuple {region; value=$1} }

unit:
  "(" ")" { {region = cover $1#region $2#region; value = ($1,$2)} }

(* Expressions *)

interactive_expr:
  expr EOF { $1 }

expr:
  switch_expr
| let_expr
| fun_expr(expr)
| local_type_decl(expr)
| local_module_decl(expr)
| local_module_alias(expr)
| conditional
| disj_expr_level { $1 }

conditional:
  if_then | if_then_else { $1 }

if_then:
  "if" test_expr branch {
    let kwd_if = $1 in
    let region = cover kwd_if#region $3.region
    and value  = {kwd_if; test=$2; ifso=$3; ifnot=None}
    in ECond {region; value} }

if_then_else:
  "if" test_expr branch "else" branch {
    let kwd_if = $1 in
    let kwd_else = $4 in
    let region = cover kwd_if#region kwd_else#region
    and value  = {kwd_if; test=$2; ifso=$3; ifnot = Some (kwd_else, $5)}
    in ECond {region; value} }

test_expr:
  braces(expr) { `Braces $1 }
| par(expr)    { `Parens $1 }

branch:
  braces(expr ";"? { $1,match $2 with Some s -> Some s | None -> None }) { $1 }

switch_expr:
  "switch" core_expr "{" cases "}" {
    let kwd_switch = $1 in
    let lbrace = $3 in
    let rbrace = $5 in
    let region = cover kwd_switch#region rbrace#region
    and value  = {kwd_switch; expr=$2; lbrace; cases=$4; rbrace}
    in ECase {region; value} }

cases:
  nseq(case_clause) {
    let hd, tl = $1 in
    let value = snd hd, tl in
    let region = nsepseq_to_region (fun x -> x.region) value
    in {region; value} }

case_clause:
  "|" pattern "=>" expr ";"? { (* TODO: keep the | and ; *)
    let start  = pattern_to_region $2
    and stop   = expr_to_region $4 in
    let region = cover start stop
    and value  = {pattern=$2; arrow=$3; rhs=$4; terminator=None }
    in $1, {region; value} }

let_expr:
  attributes "let" ioption("rec") let_binding ";" expr {
    let attributes = $1 in
    let kwd_let = $2 in
    let kwd_rec = $3 in
    let binding = $4 in
    let semi    = $5 in
    let body    = $6 in
    let stop    = expr_to_region $6 in
    let region  = cover kwd_let#region stop
    and value   = {kwd_let; kwd_rec; binding; semi; body; attributes}
    in ELetIn {region; value} }

local_type_decl(right_expr):
  type_decl ";" right_expr {
    let type_decl = $1.value in
    let semi      = $2 in
    let body      = $3 in
    let stop      = expr_to_region $3 in
    let region    = cover $1.region stop
    and value     = {type_decl; semi; body}
    in ETypeIn {region; value} }

local_module_decl(right_expr):
  module_decl ";" right_expr {
    let mod_decl  = $1.value in
    let semi      = $2 in
    let body      = $3 in
    let stop      = expr_to_region $3 in
    let region    = cover $1.region stop
    and value     = {mod_decl; semi; body}
    in EModIn {region; value} }

local_module_alias(right_expr):
  module_alias ";" right_expr {
    let mod_alias = $1.value in
    let semi      = $2 in
    let body      = $3 in
    let stop      = expr_to_region $3 in
    let region    = cover $1.region stop
    and value     = {mod_alias; semi; body}
    in EModAlias {region; value} }

fun_expr(right_expr):
  attributes ES6FUN single_fun_arg "=>" right_expr {
    let region = cover (pattern_to_region $3) (expr_to_region $5) in
    let value = {binders    = $3;
                 lhs_type   = None;
                 arrow      = $4;
                 body       = $5;
                 attributes = $1}
    in EFun {region; value}
  }
| attributes ES6FUN
  unit type_annotation? "=>" right_expr {
    let stop   = expr_to_region $6 in
    let region = cover $3.region stop in
    let binders = PPar {
      region = $3.region;
      value = {
          lpar = fst $3.value;
          inside = PUnit $3;
          rpar = snd $3.value}
      } in
    let value = {binders; lhs_type=$4; arrow=$5; body=$6; attributes=$1}
    in EFun {region; value}
  }
| attributes ES6FUN
  "(" nsepseq(fun_arg, ",") ")" type_annotation? "=>" right_expr {
    let lpar = $3 in
    let rpar = $5 in
    let arrow = $7 in
    let stop   = expr_to_region $8 in
    let region = cover lpar#region stop in
    let ptuple_region = nsepseq_to_region pattern_to_region $4 in
    let (hd, tl) = $4 in
    let binders = PPar {
      region = cover lpar#region arrow#region;
      value = {
          lpar;
          inside =
            (match tl with
               [] -> hd
             | _ -> PTuple {value = $4; region = ptuple_region});
          rpar}
      } in
    let value = {binders; lhs_type=$6; arrow; body=$8; attributes=$1}
    in EFun {region; value} }

single_fun_arg:
  var_pattern ":" type_expr {
    let start  = $1.region in
    let stop   = type_expr_to_region $3 in
    let region = cover start stop in
    let value  = {pattern=PVar $1; colon=$2; type_expr=$3}
    in PTyped {region; value}
  }
| var_pattern { PVar $1 }

fun_arg:
  sub_irrefutable type_annotation? {
    match $2 with
      Some (colon, type_expr) ->
        let region =
          cover (pattern_to_region $1) (type_expr_to_region type_expr)
        in PTyped { value = {pattern=$1; colon; type_expr}; region }
    | None -> $1 }

disj_expr_level:
  bin_op(disj_expr_level, "||", conj_expr_level)
| bin_op(disj_expr_level, "or", conj_expr_level) {
    ELogic (BoolExpr (Or $1)) }
| conj_expr_level { $1 }

bin_op(arg1,op,arg2):
  arg1 op arg2 {
    let op = $2 in
    let start  = expr_to_region $1 in
    let stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1=$1; op; arg2=$3}
    in {region; value} }

conj_expr_level:
  bin_op(conj_expr_level, "&&", comp_expr_level) {
    ELogic (BoolExpr (And $1)) }
| comp_expr_level { $1 }

comp_expr_level:
  bin_op(comp_expr_level, "<", cat_expr_level) {
    ELogic (CompExpr (Lt $1)) }
| bin_op(comp_expr_level, "<=", cat_expr_level) {
    ELogic (CompExpr (Leq $1)) }
| bin_op(comp_expr_level, ">", cat_expr_level) {
    ELogic (CompExpr (Gt $1)) }
| bin_op(comp_expr_level, ">=", cat_expr_level) {
    ELogic (CompExpr (Geq $1)) }
| bin_op(comp_expr_level, "==", cat_expr_level) {
    ELogic (CompExpr (Equal $1)) }
| bin_op(comp_expr_level, "!=", cat_expr_level) {
    ELogic (CompExpr (Neq $1)) }
| cat_expr_level { $1 }

cat_expr_level:
  bin_op(add_expr_level, "++", cat_expr_level)    {  EString (Cat $1) }
| add_expr_level                                  {                $1 }

add_expr_level:
  bin_op(add_expr_level, "+", mult_expr_level)     {  EArith (Add $1) }
| bin_op(add_expr_level, "-", mult_expr_level)     {  EArith (Sub $1) }
| mult_expr_level                                  {               $1 }

mult_expr_level:
  bin_op(mult_expr_level, "*", shift_expr_level)    { EArith (Mult $1) }
| bin_op(mult_expr_level, "/", shift_expr_level)    {  EArith (Div $1) }
| bin_op(mult_expr_level, "mod", shift_expr_level)  {  EArith (Mod $1) }
| bin_op(mult_expr_level, "land", shift_expr_level) { EArith (Land $1) }
| bin_op(mult_expr_level, "lor", shift_expr_level)  {  EArith (Lor $1) }
| bin_op(mult_expr_level, "lxor", shift_expr_level) { EArith (Lxor $1) }
| shift_expr_level                                  {               $1 }

shift_expr_level:
  bin_op(unary_expr_level, "lsl", shift_expr_level) { EArith (Lsl $1) }
| bin_op(unary_expr_level, "lsr", shift_expr_level) { EArith (Lsr $1) }
| unary_expr_level                                  {              $1 }

unary_expr_level:
  "-" call_expr_level {
    let op = $1 in
    let start = op#region in
    let stop = expr_to_region $2 in
    let region = cover start stop
    and value  = {op; arg=$2}
    in EArith (Neg {region; value})
  }
| "!" call_expr_level {
    let op = $1 in
    let start = op#region in
    let stop = expr_to_region $2 in
    let region = cover start stop
    and value  = {op; arg=$2} in
    ELogic (BoolExpr (Not {region; value}))
  }
| call_expr_level { $1 }

call_expr_level:
  call_expr  { ECall $1 }
| core_expr
| constr_expr { $1 }

constr_expr:
  "<uident>" argument {
    let constr = unwrap $1 in
    let region = cover constr.region (expr_to_region $2) in
    EConstr {region; value = (constr, Some $2)}
  }
| constant_constr_expr { $1 }

constant_constr_expr:
  "<uident>" {
    let constr = unwrap $1 in
    EConstr {constr with value=(constr,None)} }

argument:
  core_expr
| constant_constr_expr { $1 }

call_expr:
  core_expr par(nsepseq(annot_expr, ",")) {
    let start  = expr_to_region $1 in
    let stop   = $2 in
    let region = cover start stop.region
    in {region; value = $1,Multiple $2}
  }
| core_expr unit {
    let start  = expr_to_region $1 in
    let stop   = $2.region in
    let region = cover start stop
    and value  = $1, Unit $2
    in {region; value} }

core_expr:
  "<int>"                             {               EArith (Int (unwrap $1)) }
| "<mutez>"                           {             EArith (Mutez  (unwrap $1)) }
| "<nat>"                             {               EArith (Nat  (unwrap $1)) }
| "<bytes>"                           {                     EBytes (unwrap $1) }
| "<ident>"                           {                       EVar (unwrap $1) }
| projection                          {                      EProj $1 }
| module_access_e                     {                      EModA $1 }
| "<string>"                          {           EString (String (unwrap $1)) }
| "<verbatim>"                        {         EString (Verbatim (unwrap $1)) }
| unit                                {                      EUnit $1 }
| list_of(expr)                       {          EList (EListComp $1) }
| spread                              {                            $1 }
| sequence                            {                       ESeq $1 }
| record_expr                         {                    ERecord $1 }
| update_record                       {                    EUpdate $1 }
| code_inj                            {                   ECodeInj $1 }
| par(annot_expr)                     {                       EPar $1 }
| par(tuple(annot_expr))    {
    let region  = $1.region in
    let inside = ETuple {value = $1.value.inside; region} in
    EPar {value = { $1.value with inside }; region = $1.region} }

code_inj:
  "[%lang" expr "]" {
    let region = cover $1.region $3#region
    and value  = {language=$1; code=$2; rbracket=$3}
    in {region; value} }

spread:
"[" expr "," "..." expr "]" {
    let lbracket = $1 in
    let comma = $3 in
    let ellipsis = $4 in
    let rbracket = $6 in
    let region = cover lbracket#region rbracket#region
    and value : cons_expr = {lbracket; lexpr=$2; comma; ellipsis; rexpr=$5; rbracket}
    in EList (ECons {region; value})
  }

annot_expr:
  expr type_annotation? {
    match $2 with
      Some (colon, annot) ->
        let start  = expr_to_region $1
        and stop   = type_expr_to_region annot in
        let region = cover start stop
        and value  = $1, colon, annot
        in EAnnot {value; region}
    | None -> $1 }

projection:
  struct_name selection {
    let start  = $1.region in
    let stop   = nsepseq_to_region selection_to_region (snd $2) in
    let region = cover start stop
    and value  = {struct_name = $1;
                  selector    = fst $2;
                  field_path  = snd $2}
    in {region; value} }

selection:
  "[" "<int>" "]" selection {
    let r, (hd, tl) = $4 in
    let result: (selection, dot) Utils.nsepseq =
      Component (unwrap $2), (ghost, hd) :: tl
    in r, result
  }
| "." field_name selection {
    let r, (hd, tl) = $3 in
    let result: (selection, dot) Utils.nsepseq =
      FieldName $2, ($1, hd) :: tl
    in r, result
  }
| "." field_name  {    $1, (FieldName $2, []) }
| "[" "<int>" "]" { ghost, (Component (unwrap $2), []) }

module_access_e :
  module_name "." module_var_e {
    let start       = $1.region in
    let stop        = expr_to_region $3 in
    let region      = cover start stop in
    let value       = {module_name=$1; selector=$2; field=$3}
    in {region; value} }

module_var_e:
  module_access_e   { EModA $1 }
| "or"              { EVar {value="or"; region=$1#region} }
| field_name        { EVar  $1 }
| projection        { EProj $1 }

record_expr:
  "{" field_assignment more_field_assignments? "}" {
    let lbrace = $1 in
    let rbrace = $4 in
    let compound = Some (`Braces (lbrace,rbrace)) in
    let region   = cover lbrace#region rbrace#region in
    match $3 with
    | Some (comma, elts) ->
        let ne_elements = Utils.nsepseq_cons $2 comma elts in
        let value = {compound;
                     ne_elements;
                     terminator=None;
                     attributes=[]}
        in {value; region}
    | None ->
       let value = {compound;
                    ne_elements = ($2,[]);
                    terminator=None;
                    attributes=[]}
       in {value; region}
  }
| "{" field_name more_field_assignments "}" {
    let lbrace = $1 in
    let rbrace = $4 in
    let value = {
      field_name = $2;
      assignment = ghost;
      field_expr = EVar $2 } in
    let field_name = {$2 with value} in
    let comma, elts = $3 in
    let ne_elements = Utils.nsepseq_cons field_name comma elts in
    let compound = Some (`Braces (lbrace,rbrace)) in
    let region   = cover lbrace#region rbrace#region in
    let value = {compound;
                 ne_elements;
                 terminator=None;
                 attributes=[]}
    in {value; region} }


update_record:
  "{" "..." path "," sep_or_term_list(field_path_assignment,",") "}" {
    let lbrace = $1 in
    let ellipsis = $2 in
    let comma = $4 in
    let rbrace = $6 in
    let region = cover lbrace#region rbrace#region in
    let ne_elements, terminator = $5 in
    let value = {
      lbrace;
      ellipsis;
      record   = $3;
      comma;
      updates  = {value = {compound = None;
                          ne_elements;
                          terminator;
                          attributes=[]};
                 region = cover comma#region rbrace#region};
      rbrace}
    in {region; value} }

more_field_assignments:
  "," sep_or_term_list(field_assignment_punning,",") {
    let elts, _region = $2
    in $1, elts }

path:
 "<ident>"   { Name (unwrap $1) }
| projection { Path $1 }

(* Sequences *)

sequence:
  "{" series "}" {
    let lbrace = $1 in
    let rbrace = $3 in
    let region   = cover lbrace#region rbrace#region
    and compound = Some (`Braces (lbrace,rbrace)) in
    let elements = Some $2 in
    let value    = {compound; elements; terminator=None}
    in {region; value} }

series:
  seq_expr ";" series? {
    match $3 with
      Some s ->  Utils.nsepseq_cons $1 $2 s
    | None -> $1, []
  }
| last_expr { $1,[] }

last_expr_inner:
  let_in_sequence
| fun_expr(last_expr_opt_semi)
| local_type_decl(last_expr_opt_semi)
| local_module_decl(last_expr_opt_semi)
| local_module_alias(last_expr_opt_semi)
| switch_expr ";"? { $1 }

last_expr_opt_semi:
  last_expr_inner | seq_expr ";"? { $1 }

last_expr:
  last_expr_inner | seq_expr { $1 }

seq_expr:
  disj_expr_level | conditional { $1 }

let_in_sequence:
  attributes "let" ioption("rec") let_binding ";" series {
    let kwd_let = $2 in
    let kwd_rec = $3 in
    let seq      = $6 in
    let stop     = nsepseq_to_region expr_to_region seq in
    let region   = cover kwd_let#region stop in
    let compound = None in
    let elements = Some seq in
    let value    = {compound; elements; terminator=None} in
    let body     = ESeq {region; value} in
    let value    = {attributes = $1;
                    kwd_let;
                    kwd_rec;
                    binding    = $4;
                    semi     = $5;
                    body}
    in ELetIn {region; value} }

field_assignment_punning:
  (* This can only happen with multiple fields -
     one item punning does NOT work in ReasonML *)
  field_name {
    let value = {field_name = $1;
                 assignment = ghost;
                 field_expr = EVar $1}
    in {$1 with value}
  }
| field_assignment { $1 }

field_assignment:
  field_name ":" expr {
    let region = cover $1.region (expr_to_region $3)
    and value  = {field_name = $1;
                  assignment = $2;
                  field_expr = $3}
    in {region; value} }

field_path_assignment:
  path ":" expr {
    let region = cover (path_to_region $1) (expr_to_region $3)
    and value  = {field_path=$1; assignment=$2; field_expr=$3}
    in {region; value} }
