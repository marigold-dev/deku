[@@@warning "-42"]

module CST = Cst_jsligo.CST
open CST
module Region = Simple_utils.Region
open! Region
open! PPrint
module Option = Simple_utils.Option

let pp_braces printer (node : 'a braces reg) =
  let inside = node.value.inside
  in string "{" ^^ nest 1 (printer inside ^^ string "}")

let pp_brackets printer (node : 'a brackets reg) =
  let inside = node.value.inside
  in string "[" ^^ nest 1 (printer inside ^^ string "]")

let pp_nsepseq :
  'a. string -> ('a -> document) -> ('a, _ Token.wrap) Utils.nsepseq -> document =
  fun sep printer elements ->
    let elems = Utils.nsepseq_to_list elements
    and sep   = string sep ^^ break 1
    in separate_map sep printer elems


let rec print ast =
  let stmt     = Utils.nseq_to_list ast.statements in
  let stmt     = List.filter_map pp_toplevel_statement stmt in
  let app stmt = group (stmt ^^ string ";")
  in separate_map (hardline ^^ hardline) app stmt

and pp_toplevel_statement = function
  TopLevel (stmt, _) -> Some (pp_statement ?top:(Some true) stmt)
| Directive _   -> None

and pp_statement ?top = function
  SBlock      s -> pp_SBlock s
| SExpr       s -> pp_expr s
| SCond       s -> group (pp_cond_expr s)
| SReturn     s -> pp_return s
| SLet        s -> pp_let ?top s
| SConst      s -> pp_const s
| SType       s -> pp_type s
| SSwitch     s -> pp_switch s
| SBreak      _ -> string "break" ^^ hardline
| SNamespace  s -> pp_namespace ?top s
| SExport     s -> pp_export s
| SImport     s -> pp_import s
| SForOf      s -> pp_for_of s
| SWhile      s -> pp_while s

and pp_SBlock stmt =
  let print = pp_nsepseq ";" pp_statement
  in group (pp_braces print stmt)

and pp_for_of {value; _} =
  string "for" ^^ string "("
  ^^ pp_index_kind value.index_kind
  ^^ string value.index.value
  ^^ string " of "
  ^^ pp_expr value.expr
  ^^ string ")"
  ^/^ pp_statement value.statement

and pp_index_kind = function
  `Let _ -> string "let "
| `Const _ -> string "const "

and pp_while {value; _} =
  string "while" ^^ string "(" ^^ pp_expr value.expr ^^ string ")" ^^ pp_statement value.statement

and pp_import (node : CST.import Region.reg) =
  let {value; _} : CST.import Region.reg = node in
  string "import" ^^ string value.alias.value
  ^^ string "="
  ^^ pp_nsepseq "." (fun a -> string a.Region.value) value.module_path

and pp_export {value = (_, statement); _} =
  string "export " ^^ pp_statement statement

and pp_namespace ?top {value = (_, name, statements, attributes); _} =
  let top = match top with
    Some true -> true
  | _ -> false
  in
  let is_private = List.exists (fun a -> a.value = "private") attributes in
  let attributes  = filter_private attributes in
  let pp_statements = pp_nsepseq ";" pp_statement in
  (if attributes = [] then empty else pp_attributes attributes) ^^ 
  string "namespace " ^^ string name.value
  ^^ (if ((top && is_private) || not top) then string "" else string "export ") 
  ^^ group (pp_braces pp_statements statements)

and pp_cond_expr {value; _} =
  let {test; ifso; ifnot; _} = value in
  let if_then = string "if" ^^ pp_par_expr test ^^ string " " ^^ pp_statement ifso in
  match ifnot with
    None -> if_then
  | Some (_,statement) ->
      if_then ^^ string " else " ^^ pp_statement statement

and pp_return {value = {expr; _}; _} =
  match expr with
    Some s -> string "return " ^^ pp_expr s
  | None -> string "return"

and filter_private (attributes: CST.attributes) : CST.attributes = 
  List.filter (fun (v: CST.attribute) -> not (v.value = "private")) attributes

and pp_let ?top (node : let_decl reg) =
  let {attributes; bindings; _} : let_decl = node.value in
  let top = match top with
    Some true -> true
  | _ -> false
  in
  let is_private = List.exists (fun a -> a.value = "private") attributes in
  let attributes  = filter_private attributes in
  (if attributes = [] then empty else pp_attributes attributes)
     ^^ (if ((top && is_private) || not top) then string "" else string "export ") 
     ^^ string "let " ^^ pp_nsepseq "," pp_val_binding bindings

and pp_const {value = {bindings; _}; _} =
  string "const " ^^ pp_nsepseq "," pp_val_binding bindings

and pp_val_binding {value = {binders; lhs_type; expr; _}; _} =
  prefix 2 0 ((match lhs_type with
    Some (_, type_expr) -> pp_pattern binders ^^ string ": " ^^ pp_type_expr type_expr
  | None -> pp_pattern binders)
  ^^
  string " = "
  )

  (pp_expr expr)

and pp_switch {value = {expr; cases; _}; _} =
  string "switch(" ^^
  pp_expr expr ^^
  string ") {" ^^
  pp_cases cases ^^
  string "}"

and pp_cases cases =
  Utils.nseq_foldl (fun a i -> a ^^ pp_case i) empty cases

and pp_case = function
  Switch_case {expr; statements; _} ->
    string "case " ^^ pp_expr expr ^^ string ":" ^^
      (match statements with
         Some s ->
          let app s = group (pp_statement s) in
          separate_map (hardline ^^ hardline) app (Utils.nsepseq_to_list s)
       | None -> empty )
| Switch_default_case {statements; _} ->
    string "default: " ^^
    (match statements with
      Some s ->
      let app s = group (pp_statement s) in
      separate_map (hardline ^^ hardline) app (Utils.nsepseq_to_list s)
    | None -> empty)


and pp_type {value; _} =
  let ({name; params; type_expr; _}: type_decl) = value
  in
  string "type " ^^ string name.value
  ^^ pp_type_params params
  ^^ string " = "
  ^^ group (nest 2 (break 1 ^^ pp_type_expr type_expr))

and pp_type_params = function
  None -> empty
| Some {value; _} ->
   string "<" ^^ nest 1 (pp_nsepseq "," pp_ident value.inside) ^^ string ">"

and pp_ident (node : string Region.reg) = string node.value

and pp_string s = string "\"" ^^ pp_ident s ^^ string "\""

and pp_verbatim s = string "`" ^^ pp_ident s ^^ string "`"

and pp_bytes (byte: (string * Hex.t) reg)  =
  let _, hex = byte.Region.value
  in string ("0x" ^ Hex.show hex)

and pp_expr = function
  EFun     e -> pp_fun e
| EPar     e -> pp_par_expr e.value
| ESeq     e -> pp_seq e
| EVar     v -> pp_ident v
| EModA    e -> pp_module_access pp_expr e
| ELogic   e -> pp_logic_expr e
| EArith   e -> group (pp_arith_expr e)
| ECall    e -> pp_call_expr e
| EBytes   e -> pp_bytes e
| EArray   e -> pp_array e
| EObject  e -> group (pp_object_expr e)
| EString  e -> pp_string_expr e
| EProj    e -> pp_projection e
| EAssign     (a,b,c) -> pp_assign (a,b,c)
| EAnnot   e -> pp_annot_expr e
| EConstr  e -> pp_constr_expr e
| EUnit    _ -> string "unit"
| ECodeInj _ -> failwith "TODO: ECodeInj"

and pp_array (node: (array_item, comma) Utils.sepseq brackets reg) =
  match node.value.inside with 
    Some node -> 
      let pp_items = pp_nsepseq "," pp_array_item in
      let result = string "[" ^^ nest 1 (pp_items node ^^ string "]") in
      group result
      (* pp_brackets (fun _ -> empty) node *)
  | None -> 
      pp_brackets (fun _ -> empty) node

and pp_call_expr {value; _} =
  let lambda, arguments = value in
  let arguments =
    match arguments with
    | Unit _ -> []
    | Multiple xs -> Utils.nsepseq_to_list xs.value.inside in
  let arguments =
    string "("
    ^^ group (separate_map (string ", ") pp_expr arguments)
    ^^ string ")"
  in pp_expr lambda ^^ arguments

and pp_array_item = function
  Expr_entry e -> pp_expr e
| Rest_entry {value = {expr; _}; _} -> string "..." ^^ pp_expr expr

and pp_constr_expr {value; _} =
  let constr, arg = value in
  let constr = string constr.value in
  match arg with
      None -> constr ^^ string "()"
  | Some e -> prefix 2 1 constr (string "(" ^^ pp_expr e ^^ string ")")

and pp_object_property = function
  Punned_property {value; _} ->
    pp_expr value
| Property {value = {name; value; _}; _} ->
    pp_expr name ^^ string ": " ^^ pp_expr value
| Property_rest {value = {expr; _}; _} ->
    string "..." ^^ pp_expr expr

and pp_object_expr (node: (property, comma) Utils.nsepseq braces reg) =
  let pp_properties = pp_nsepseq "," pp_object_property
  in pp_braces pp_properties node

and pp_string_expr = function
  String e -> pp_string e
| Verbatim e -> pp_verbatim e

and pp_selection = function
  FieldName {value = {value;_ }; _} -> string "." ^^ pp_ident value
| Component {value = {inside; _}; _} -> string "[" ^^ pp_expr inside ^^ string "]"

and pp_projection {value = {expr; selection}; _} =
  pp_expr expr ^^ pp_selection selection

and pp_assign (a, op, b) =
  let operator = match op.value with 
      Eq -> " = "
    | Assignment_operator Times_eq ->  " *= "
    | Assignment_operator Div_eq ->    " /= "
    | Assignment_operator Min_eq ->    " -= "
    | Assignment_operator Plus_eq ->   " += "
    | Assignment_operator Mod_eq ->    " %= "
  in
  pp_expr a ^^ string operator ^^ pp_expr b

and pp_annot_expr {value; _} =
  let expr, _, type_expr = value in
    group (nest 1 (pp_expr expr ^/^ string "as "
    ^^ pp_type_expr type_expr))

and pp_logic_expr = function
  BoolExpr e -> pp_bool_expr e
| CompExpr e -> pp_comp_expr e

and pp_bool_expr = function
  Or   e  -> pp_bin_op "||" e
| And  e  -> pp_bin_op "&&" e
| Not  e  -> pp_un_op "!" e

and pp_bin_op op {value; _} =
  let {arg1; arg2; _} = value
  and length = String.length op + 1 in
  pp_expr arg1 ^^ string " " ^^ string (op ^ " ") ^^ nest length (pp_expr arg2)

and pp_un_op op {value; _} =
  string op ^^ pp_expr value.arg

and pp_comp_expr = function
  Lt    e -> pp_bin_op "<"  e
| Leq   e -> pp_bin_op "<=" e
| Gt    e -> pp_bin_op ">"  e
| Geq   e -> pp_bin_op ">=" e
| Equal e -> pp_bin_op "=="  e
| Neq   e -> pp_bin_op "!=" e

and pp_arith_expr = function
  Add   e -> pp_bin_op "+" e
| Sub   e -> pp_bin_op "-" e
| Mult  e -> pp_bin_op "*" e
| Div   e -> pp_bin_op "/" e
| Mod   e -> pp_bin_op "%" e
| Neg   e -> string "-" ^^ pp_expr e.value.arg
| Int   e -> pp_int e

and pp_int {value; _} =
  string (Z.to_string (snd value))

and pp_par_expr value =
  string "(" ^^ nest 1 (pp_expr value.inside ^^ string ")")

and pp_expr_fun = function
  EPar {value; _} ->
    string "(" ^^ nest 1 (pp_expr_fun value.inside ^^ string ")")
| ESeq {value; _} ->
    group (pp_nsepseq "," pp_expr_fun value)
| EAnnot   {value; _} ->
    let expr, _, type_expr = value in
    group (nest 1 (pp_expr_fun expr ^^ string ": "
                   ^^ pp_type_expr type_expr))
| EUnit _ -> string "()"
| _ as c -> pp_expr c

and pp_fun {value; _} =
  let {parameters; lhs_type; body; _} = value in
  let parameters = pp_expr_fun parameters
  and annot   =
    match lhs_type with
      None -> empty
    | Some (_,e) ->
       string ": " ^^ nest 2 (pp_type_expr e)
  in
  match body with
  | FunctionBody fb ->
      let pp_statements = pp_nsepseq ";" pp_statement in
      parameters ^^ annot ^^ string " => "
      ^^ (pp_braces pp_statements fb)
  | ExpressionBody e ->
     prefix 2 0 (nest 1 parameters ^^ annot ^^ string " => ")
                (pp_expr e)

and pp_seq {value; _} =
  pp_nsepseq "," pp_expr value

and pp_type_expr: type_expr -> document = function
  TProd   t -> pp_cartesian t
| TSum    t -> break 0 ^^ pp_sum_type t
| TObject t -> pp_object_type t
| TApp    t -> pp_type_app t
| TFun    t -> pp_fun_type t
| TPar    t -> pp_type_par t
| TVar    t -> pp_ident t
| TString s -> pp_string s
| TModA   t -> pp_module_access pp_type_expr t
| TInt    t -> pp_int t

and pp_module_access : type a.(a -> document) -> a module_access reg -> document
= fun f {value; _} ->
  let {module_name; field; _} = value in
  group (pp_ident module_name ^^ string "." ^^ break 0 ^^ f field)

 and pp_cartesian (node: CST.cartesian) =
  let pp_type_exprs = pp_nsepseq "," pp_type_expr
  in group (
    pp_attributes node.attributes ^^ hardline ^^
    pp_brackets pp_type_exprs node.inside)

 and pp_sum_type (node : sum_type reg) =
  let {variants; attributes; _} = node.value in
  let variants = pp_nsepseq "|" pp_variant variants.value in
  if attributes = [] then variants
  else group (pp_attributes attributes ^/^ variants)

and pp_variant (node : variant reg) =
  let {tuple; attributes; _} = node.value in
  let comp = tuple.value.inside in
  let tuple =
    string "[" ^^ nest 1 (pp_variant_comp comp ^^ string "]") in
  if attributes = [] then tuple
  else group (pp_attributes attributes ^/^ tuple)

and pp_variant_comp (node: variant_comp) =
  let {constr; params} = node in
  let constr, params =
    match params with
      None -> pp_string constr, []
    | Some (_comma, params) ->
       pp_string constr , Utils.nsepseq_to_list params in
  let sep = string "," ^^ break 1
  in constr ^^ sep ^^ separate_map sep pp_type_expr params

and pp_attributes = function
  [] -> empty
| attr ->
  let make s = string "@" ^^ string s.Region.value ^^ string " "
  in
  string "/* " ^^ concat_map make attr ^^ string "*/ "

and pp_object_type fields = group (pp_ne_injection pp_field_decl fields)

and pp_field_decl {value; _} =
  let {field_name; field_type; attributes; _} = value in
  let attr = pp_attributes attributes in
  let name = if attributes = [] then pp_ident field_name
             else attr ^/^ pp_ident field_name in
  match field_type with
    TVar v when v = field_name -> name
  | _ -> let t_expr = pp_type_expr field_type
        in prefix 2 1 (name ^^ string ":") t_expr

and pp_ne_injection :
  'a.('a -> document) -> 'a ne_injection reg -> document =
  fun printer {value; _} ->
    let {compound; ne_elements; attributes; _} = value in
    let elements = pp_nsepseq "," printer ne_elements in
    let inj =
      match Option.map ~f:pp_compound compound with
        None -> elements
      | Some (opening, closing) ->
          string opening ^^ nest 2 (break 0 ^^ elements)
          ^^ break 0 ^^ string closing in
    let inj = if attributes = [] then inj
              else break 0 ^^ pp_attributes attributes ^/^ inj
    in inj

and pp_compound = function
| Braces   (_, _) -> ("{","}")
| Brackets (_, _) -> ("[","]")

and pp_type_app {value; _} =
  let ctor, tuple = value in
  pp_ident ctor
  ^^ string "<" ^^ nest 1 (pp_type_tuple tuple) ^^ string ">"

and pp_type_tuple {value; _} =
  let head, tail = value.inside in
  let rec app = function
    []  -> empty
  | [e] -> group (break 1 ^^ pp_type_expr e)
  | e::items ->
      group (break 1 ^^ pp_type_expr e ^^ string ",") ^^ app items in
  if tail = []
  then pp_type_expr head
  else
    let components =
      pp_type_expr head ^^ string "," ^^ app (List.map snd tail)
    in components

and pp_fun_type_arg ({name; type_expr; _} : CST.fun_type_arg) =
  pp_ident name ^^ string ":" ^^ pp_type_expr type_expr

and pp_fun_type {value; _} =
  let lhs, _, rhs = value in
  group ( nest 1 (string "(" ^^ pp_nsepseq "," pp_fun_type_arg lhs.inside ^^ string ")") ^^ string " =>" ^/^ pp_type_expr rhs)

and pp_type_par {value; _} =
  string "(" ^^ nest 1 (pp_type_expr value.inside ^^ string ")")

and pp_pattern = function
  PRest     p -> pp_rest_pattern p
| PAssign   p -> pp_assign_pattern p
| PVar      v -> pp_pvar v
| PConstr   p -> pp_ident p
| PDestruct p -> pp_destruct p
| PObject   p -> pp_pobject p
| PArray    p -> pp_parray p

and pp_parray (node:  (pattern, comma) Utils.nsepseq brackets reg) =
  let pp_patterns = pp_nsepseq "," pp_pattern
  in group (pp_brackets pp_patterns node)

and pp_pobject (node: (pattern, comma) Utils.nsepseq braces reg) =
  pp_braces (pp_nsepseq "," pp_pattern) node

and pp_pvar {value; _} =
  let {variable; attributes} = value in
  let v = pp_ident variable in
  if attributes = [] then v
  else group (pp_attributes attributes ^/^ v)

and pp_rest_pattern {value = {rest; _}; _} =
  string "..." ^^ pp_ident rest

and pp_assign_pattern {value = {property; value; _}; _} =
  pp_ident property ^^ string "=" ^^ pp_expr value

and pp_destruct {value = {property; target; _}; _} =
  pp_ident property ^^ string ":" ^^ pp_val_binding target

let print_type_expr = pp_type_expr
let print_pattern   = pp_pattern
let print_expr      = pp_expr

type cst       = CST.t
type expr      = CST.expr
type type_expr = CST.type_expr
type pattern   = CST.pattern
