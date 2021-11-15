[@@@warning "-42"]

module CST = Cst_reasonligo.CST
open CST
module Region = Simple_utils.Region
open! Region
open! PPrint
module Option = Simple_utils.Option
(*module Directive = LexerLib.Directive*)

let pp_par printer (node : 'a par reg) =
  let inside = node.value.inside
  in string "(" ^^ nest 1 (printer inside ^^ string ")")

let pp_braces printer (node : 'a braces reg) =
  let inside = node.value.inside
  in string "{" ^^ nest 1 (printer inside ^^ string "}")

let rec print ast =
  let decl = Utils.nseq_to_list ast.decl in
  let decl = List.filter_map pp_declaration decl
  in separate_map (hardline ^^ hardline) group decl

and pp_declaration = function
  ConstDecl   decl -> Some (pp_const_decl decl)
| TypeDecl    decl -> Some (pp_type_decl  decl)
| ModuleDecl  decl -> Some (pp_module_decl  decl)
| ModuleAlias decl -> Some (pp_module_alias decl)
| Directive      _ -> None

(*
and pp_dir_decl = function
  Directive.Linemarker {value; _} ->
    let open Directive in
    let linenum, file_path, flag_opt = value in
    let flag =
      match flag_opt with
        Some Push -> " 1"
      | Some Pop  -> " 2"
      | None      -> "" in
    let lexeme = Printf.sprintf "# %d %S%s" linenum file_path flag
    in string lexeme
*)

and pp_const_decl = function
| {value = (_,rec_opt, binding, attr); _} ->
  let let_str =
    match rec_opt with
        None -> "let "
    | Some _ -> "let rec " in
  let bindings = pp_let_binding let_str binding in
  let bindings = if attr = [] then bindings
                 else pp_attributes attr ^/^ bindings
  in group (bindings ^^ string ";")

and pp_attributes = function
    [] -> empty
| attr ->
   let make s = string "[@" ^^ string s.value ^^ string "]"
   in concat_map make attr

and pp_ident t = string t.value

and pp_string s = string "\"" ^^ pp_ident s ^^ string "\""

and pp_verbatim s = string "{|" ^^ pp_ident s ^^ string "|}"

and pp_let_binding let_ (binding : let_binding) =
  let {binders; lhs_type; let_rhs; _} = binding in
  let patterns = group (pp_pattern binders) in
  let lhs =
    string let_ ^^
      match lhs_type with
        None -> patterns ^^ string " = "
      | Some (_,e) ->
          patterns ^^ group (break 0 ^^ string ": " ^^ pp_type_expr e ^^ string " = ")
  in
  let rhs = pp_expr let_rhs in
  match let_rhs with
  | EFun _
  | ESeq _
  | ERecord _ -> lhs ^^ rhs
  | _ -> prefix 2 0 lhs rhs

and pp_pattern = function
  PConstr p -> pp_pconstr p
| PUnit   _ -> string "()"
| PVar    v -> pp_pvar v
| PInt    i -> pp_int i
| PNat    n -> pp_nat n
| PBytes  b -> pp_bytes b
| PString s -> pp_string s
| PVerbatim s -> pp_verbatim s
| PList   l -> pp_plist l
| PTuple  t -> pp_ptuple t
| PPar    p -> pp_ppar p
| PRecord r -> pp_precord r
| PTyped  t -> pp_ptyped t

and pp_pvar {value; _} =
  let {variable; attributes} = value in
  let v = pp_ident variable in
  if attributes = [] then v
  else group (pp_attributes attributes ^/^ v)

and pp_pconstr {value; _} =
  match value with
    constr, None -> pp_ident constr
  | constr, Some (PVar _ as pat) ->
      prefix 2 1 (pp_ident constr)  (pp_pattern pat)
  | constr, Some (_ as pat)->
      prefix 2 0 (pp_ident constr)  (pp_pattern pat)

and pp_int {value; _} =
  string (Z.to_string (snd value))

and pp_nat {value; _} =
  string (Z.to_string (snd value) ^ "n")

and pp_bytes {value; _} =
  string ("0x" ^ Hex.show (snd value))

and pp_ppar {value; _} =
  string "(" ^^ nest 1 (pp_pattern value.inside) ^^ string ")"

and pp_plist = function
  PListComp cmp -> pp_list_comp cmp
| PCons cons -> pp_cons cons

and pp_list_comp e = group (pp_injection pp_pattern e)

and pp_cons {value; _} =
  let {lpattern;rpattern;_} = value in
  string "[" ^^ (pp_pattern lpattern ^^ string ", ") ^^ group ( break 0 ^^ string "..." ^^ pp_pattern rpattern) ^^ string "]"

and pp_ptuple {value; _} =
  let head, tail = value in
  let rec app = function
    []  -> empty
  | [p] -> group (break 1 ^^ pp_pattern p)
  | p::items ->
      group (break 1 ^^ pp_pattern p ^^ string ",") ^^ app items
  in if tail = []
     then nest 1 (pp_pattern head)
     else nest 1 (pp_pattern head ^^ string "," ^^ app (List.map snd tail))

and pp_precord fields = pp_ne_injection pp_field_pattern fields

and pp_field_pattern {value; _} =
  let {field_name; pattern; _} = value in
  prefix 2 1 (pp_ident field_name ^^ string " =") (pp_pattern pattern)

and pp_ptyped {value; _} =
  let {pattern; type_expr; _} = value in
  group (pp_pattern pattern ^^ string ": " ^^ pp_type_expr type_expr)

and pp_type_decl decl =
  let {name; params; type_expr; _} = decl.value in
  string "type " ^^ pp_ident name
  ^^ pp_type_params params
  ^^ string " = "
  ^^ group (pp_type_expr type_expr) ^^ string ";"

and pp_type_params = function
  None -> empty
| Some {value; _} ->
    let vars = pp_nsepseq "," pp_quoted_param value.inside
    in string "(" ^^ nest 1 (vars ^^ string ")")

and pp_quoted_param param =
  pp_ident {param with value = "'" ^ param.value.name.value}

and pp_module_decl decl =
  let {name; module_; _} = decl.value in
  string "module " ^^ pp_ident name ^^ string " = "
  ^^ group (print module_) ^^ string ";"

and pp_module_alias decl =
  let {alias; binders; _} = decl.value in
  string "module " ^^ string alias.value ^^ string " = "
  ^^ group (pp_nsepseq "." pp_ident binders) ^^ string ";"

and pp_expr = function
  ECase       e -> pp_case_expr e
| ECond       e -> group (pp_cond_expr e)
| EAnnot      e -> pp_annot_expr e
| ELogic      e -> pp_logic_expr e
| EArith      e -> group (pp_arith_expr e)
| EString     e -> pp_string_expr e
| EList       e -> group (pp_list_expr e)
| EConstr     e -> pp_constr_expr e
| ERecord     e -> pp_record_expr e
| EProj       e -> pp_projection e
| EModA       e -> pp_module_access pp_expr e
| EUpdate     e -> pp_update e
| EVar        v -> pp_ident v
| ECall       e -> pp_call_expr e
| EBytes      e -> pp_bytes e
| EUnit       _ -> string "()"
| ETuple      e -> pp_tuple_expr e
| EPar        e -> pp_par pp_expr e
| ELetIn      e -> pp_let_in e
| ETypeIn     e -> pp_type_in e
| EModIn      e -> pp_mod_in e
| EModAlias   e -> pp_mod_alias e
| EFun        e -> pp_fun e
| ESeq        e -> pp_seq e
| ECodeInj e -> pp_code_inj e

and pp_case_expr_switch s e =
  match e with
    EVar _ -> prefix 2 1 s (pp_expr e)
  | _      -> s ^^ pp_expr e

and pp_case_expr {value; _} =
  let {expr; cases; _} = value in
  group ((pp_case_expr_switch (string "switch") expr) ^^ string "{"
         ^^ pp_cases cases ^^ hardline ^^ string "}")

and pp_cases {value; _} =
  let head, tail = value in
  let rest = List.map snd tail in
  let app clause = break 1 ^^ string "| " ^^ pp_clause clause
  in  concat_map app (head :: rest)

and pp_clause {value; _} =
  let {pattern; rhs; _} = value in
  prefix 4 1 (pp_pattern pattern ^^ string " =>") (pp_expr rhs)

and pp_cond_expr {value; _} =
  let {test; ifso; ifnot; _} = value in
  let if_then =
    string "if" ^^ pp_test_expr test ^^ string " {" ^^ break 0
    ^^ group (nest 2 (break 2 ^^ pp_expr (fst ifso.value.inside))) ^^ hardline ^^ string "}" in
  match ifnot with
    None -> if_then
  | Some (_,branch) ->
      let expr = fst branch.value.inside in
      if_then
      ^^ string " else"
      ^^ string " {" ^^ break 0
      ^^ group (nest 2 (break 2 ^^ pp_expr expr ^^ hardline) ^^ string "}")

and pp_test_expr = function
  `Parens e -> pp_par pp_expr e
| `Braces e -> pp_braces pp_expr e

and pp_annot_expr {value; _} =
  let expr, _, type_expr = value in
    group (nest 1 (pp_expr expr ^/^ string ": "
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
  string (op ^ " ") ^^ pp_expr value.arg

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
| Mod   e -> pp_bin_op "mod" e
| Land  e -> pp_bin_op "land" e
| Lor   e -> pp_bin_op "lor" e
| Lxor  e -> pp_bin_op "lxor" e
| Lsl   e -> pp_bin_op "lsl" e
| Lsr   e -> pp_bin_op "lsr" e
| Neg   e -> string "-" ^^ pp_expr e.value.arg
| Int   e -> pp_int e
| Nat   e -> pp_nat e
| Mutez e -> pp_mutez e

and pp_mutez {value; _} =
  Z.to_string (snd value) ^ "mutez" |> string

and pp_string_expr = function
     Cat e -> pp_bin_op "++" e
| String e -> pp_string e
| Verbatim e -> pp_verbatim e

and pp_list_expr = function
| ECons {value = {lexpr; rexpr; _}; _ } ->
  string "[" ^^ pp_expr lexpr ^^ string "," ^^ break 1 ^^ string "..." ^^ pp_expr rexpr ^^ string "]"
| EListComp e -> group (pp_injection pp_expr e)

and pp_injection :
  'a.('a -> document) -> 'a injection reg -> document =
  fun printer {value; _} ->
    let {compound; elements; _} = value in
    let sep = (string ",") ^^ break 1 in
    let elements = Utils.sepseq_to_list elements in
    let elements = separate_map sep printer elements in
    match Option.map ~f:pp_compound compound with
      None -> elements
    | Some (opening, closing) ->
        string opening ^^ nest 1 elements ^^ string closing

and pp_compound = function
| `Braces   (_, _) -> ("{","}")
| `Brackets (_, _) -> ("[","]")

and pp_constr_expr {value; _} =
  let constr, arg = value in
  let constr = string constr.value in
  match arg with
      None -> constr
  | Some e -> prefix 2 1 constr (pp_expr e)

and pp_record_expr ne_inj = pp_ne_injection pp_field_assign ne_inj

and pp_field_assign {value; _} =
  let {field_name; field_expr; _} = value in
  prefix 2 1 (pp_ident field_name ^^ string ":") (pp_expr field_expr)

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
          ^^ break 1 ^^ string closing in
    let inj = if attributes = [] then inj
              else break 0 ^^ pp_attributes attributes ^/^ inj
    in inj

and pp_nsepseq :
  'a.string -> ('a -> document) -> ('a, _ Token.wrap) Utils.nsepseq -> document =
  fun sep printer elements ->
    let elems = Utils.nsepseq_to_list elements
    and sep   = string sep ^^ break 1
    in separate_map sep printer elems

and pp_projection {value; _} =
  let {struct_name; field_path; _} = value in
  let subpath = Utils.nsepseq_to_list field_path in
  let subpath = concat_map pp_selection subpath in
  group (pp_ident struct_name ^^ subpath)

and pp_module_access : type a.(a -> document) -> a module_access reg -> document
= fun f {value; _} ->
  let {module_name; field; _} = value in
  group (pp_ident module_name ^^ string "." ^^ break 0 ^^ f field)

and pp_selection = function
  FieldName v   -> string "." ^^ break 0 ^^ string v.value
| Component cmp ->
    string "[" ^^ (cmp.value |> snd |> Z.to_string |> string) ^^ string "]"

and pp_update {value; _} =
  let {record; updates; _} = value in
  let updates = group (pp_ne_injection pp_field_path_assign updates)
  and record  = pp_path record in
  string "{..." ^^ record ^^ string ","
  ^^ nest 2 (break 1 ^^ updates ^^ string "}")

and pp_code_inj {value; _} =
  let {language; code; _} = value in
  let language = string language.value.value
  and code     = pp_expr code in
  string "[%" ^^ language ^/^ code ^^ string "]"

and pp_field_path_assign {value; _} =
  let {field_path; field_expr; _} = value in
  let path = pp_path field_path in
  prefix 2 1 (path ^^ string ":") (pp_expr field_expr)

and pp_path = function
  Name v -> pp_ident v
| Path p -> pp_projection p

and pp_call_expr {value; _} =
  let lambda, arguments = value in
  let arguments =
    match arguments with
    | Unit _ -> []
    | Multiple xs -> Utils.nsepseq_to_list xs.value.inside in
  let arguments = string "(" ^^ group (separate_map (string "," ^^ break 0 ^^ string " ") pp_expr arguments) ^^ string ")" in
  group (break 0 ^^ pp_expr lambda ^^ nest 2 arguments)

and pp_tuple_expr {value; _} =
  let head, tail = value in
  let rec app = function
    []  -> empty
  | [e] -> group (break 1 ^^ pp_expr e)
  | e::items ->
      group (break 1 ^^ pp_expr e ^^ string ",") ^^ app items
  in if tail = []
     then nest 1 (pp_expr head)
     else nest 1 (pp_expr head ^^ string "," ^^ app (List.map snd tail))

and pp_let_in {value; _} =
  let {binding; kwd_rec; body; attributes; _} = value in
  let let_str =
    match kwd_rec with
        None -> "let "
    | Some _ -> "let rec " in
  let bindings = pp_let_binding let_str binding
  and attr    = pp_attributes attributes
  in attr ^^ bindings
     ^^ string ";" ^^ hardline ^^ pp_expr body

and pp_type_in {value; _} =
  let {type_decl; body; _} = value in
  let {name; type_expr; _} = type_decl
  in string "type "
     ^^ prefix 2 1 (pp_ident name ^^ string " =")
                   (pp_type_expr type_expr)
     ^^ string ";" ^^ hardline ^^ pp_expr body

and pp_mod_in {value; _} =
  let {mod_decl; body; _} = value in
  let {name; module_; _} = mod_decl
  in string "module"
     ^^ prefix 2 1 (pp_ident name ^^ string "= {")
                   (print module_)
     ^^ string " }"
     ^^ string ";" ^^ hardline ^^ group (pp_expr body)

and pp_mod_alias {value; _} =
  let {mod_alias; body; _} = value in
  let {alias; binders; _} = mod_alias
  in string "module"
     ^^ prefix 2 1 (pp_ident alias ^^ string "= {")
                   (pp_nsepseq "." pp_ident binders)
     ^^ string " }"
     ^^ string ";" ^^ hardline ^^ group (pp_expr body)

and pp_fun {value; _} =
  let {binders; lhs_type; body; _} = value in
  let binders = pp_pattern binders
  and annot   =
    match lhs_type with
      None -> empty
    | Some (_,e) ->
        group (break 0 ^^ string ": " ^^ nest 2 (pp_type_expr e))
  in
  match body with
  | ESeq _ -> nest 1 binders ^^ annot ^^ string " => " ^^ pp_expr body
  | _ -> (prefix 2 0 (nest 1 binders ^^ annot ^^ string " => ") (pp_expr body))

and pp_seq {value; _} =
  let {compound; elements; _} = value in
  let sep = string ";" ^^ hardline in
  let elements = Utils.sepseq_to_list elements in
  let elements = separate_map sep pp_expr elements in
  match Option.map ~f:pp_compound compound with
    None -> elements
  | Some (opening, closing) ->
     string opening
     ^^ nest 2 (hardline ^^ elements) ^^ hardline
     ^^ string closing

and pp_type_expr = function
  TProd t   -> pp_cartesian t
| TSum t    -> break 0 ^^ pp_sum_type t
| TRecord t -> pp_record_type t
| TApp t    -> pp_type_app t
| TFun t    -> pp_fun_type t
| TPar t    -> pp_type_par t
| TVar t    -> pp_ident t
| TString s -> pp_string s
| TInt    i -> pp_int i
| TModA   t -> pp_module_access pp_type_expr t
| TArg    t -> pp_quoted_param t

and pp_cartesian {value; _} =
  let head, tail = value.inside in
  let rec app = function
    []  -> empty
  | [e] -> group (break 1 ^^ pp_type_expr e)
  | e::items ->
      group (break 1 ^^ pp_type_expr e ^^ string ",") ^^ app items
  in
  string "(" ^^ nest 1 (pp_type_expr head ^^ (if tail <> [] then string "," else empty) ^^ app (List.map snd tail)) ^^ string ")"

and pp_sum_type {value; _} =
  let {variants; attributes; _} = value in
  let head, tail = variants in
  let head = pp_variant head in
  let padding_flat =
    if attributes = [] then empty else string "| " in
  let padding_non_flat =
    if attributes = [] then blank 2 else string "| " in
  let head =
    if tail = [] then head
    else ifflat (padding_flat ^^ head) (padding_non_flat ^^ head) in
  let rest = List.map snd tail in
  let app variant = break 1 ^^ string "| " ^^ pp_variant variant in
  let whole = head ^^ concat_map app rest in
  if attributes = [] then whole
  else pp_attributes attributes ^/^ whole

and pp_variant {value; _} =
  let {constr; args; attributes=attr} = value in
  let pre = if attr = [] then pp_ident constr
            else group (pp_attributes attr ^/^ pp_ident constr) in
  match args with
    None -> pre
  | Some {value={inside=e; _}; _} ->
      prefix 2 0 pre (string "(" ^^ pp_type_expr e ^^ string ")")

and pp_record_type fields = group (pp_ne_injection pp_field_decl fields)

and pp_field_decl {value; _} =
  let {field_name; field_type; attributes; _} = value in
  let attr = pp_attributes attributes in
  let name = if attributes = [] then pp_ident field_name
             else attr ^/^ pp_ident field_name in
  match field_type with
    TVar v when v = field_name -> name
  | _ -> let t_expr = pp_type_expr field_type
        in prefix 2 1 (name ^^ string ":") t_expr

and pp_type_app {value; _} =
  let ctor, tuple = value in
  prefix 2 0 (pp_type_constr ctor) (string "(" ^^ nest 1 (pp_type_tuple tuple) ^^ string ")")

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

and pp_type_constr ctor = string ctor.value

and pp_fun_type {value; _} =
  let lhs, _, rhs = value in
  group ( nest 1 (pp_type_expr lhs) ^^ string " =>" ^/^ pp_type_expr rhs)

and pp_type_par {value; _} =
  string "(" ^^ nest 1 (pp_type_expr value.inside ^^ string ")")

let print_type_expr = pp_type_expr
let print_pattern   = pp_pattern
let print_expr      = pp_expr

type cst       = CST.t
type expr      = CST.expr
type type_expr = CST.type_expr
type pattern   = CST.pattern
