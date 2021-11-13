[@@@warning "-42"]
[@@@coverage exclude_file]

open CST

module Directive = LexerLib.Directive
module Region = Simple_utils.Region
open! Region
module Utils = Simple_utils.Utils

let sprintf = Printf.sprintf

let ghost = 
  object 
    method region = Region.ghost 
    method attributes = []
    method payload = ""
  end 

type state = <
  offsets  : bool;
  mode     : [`Point | `Byte];
  buffer   : Buffer.t;
  pad_path : string;
  pad_node : string;
  pad      : int -> int -> state
>

let mk_state ~offsets ~mode ~buffer =
  object
    method offsets  = offsets;
    method mode     = mode;
    method buffer   = buffer
    val pad_path    = ""
    method pad_path = pad_path
    val pad_node    = ""
    method pad_node = pad_node

    (** The method [pad] updates the current padding, which is
        comprised of two components: the padding to reach the new node
        (space before reaching a subtree, then a vertical bar for it)
        and the padding for the new node itself (Is it the last child
        of its parent?).
     *)
    method pad arity rank =
      {< pad_path =
           pad_node ^ (if rank = arity-1 then "`-- " else "|-- ");
         pad_node =
           pad_node ^ (if rank = arity-1 then "    " else "|   ")
      >}
  end

let compact state (region: Region.t) =
  region#compact ~offsets:state#offsets state#mode

(** {1 Printing the tokens with their source regions} *)

let print_nsepseq :
  state -> string -> (state -> 'a -> unit) ->
  ('a, _ Token.wrap) Utils.nsepseq -> unit =
  fun state sep print (head, tail) ->
    let print_aux (sep_reg, item) =
      let sep_line =
        sprintf "%s: %s\n" (compact state sep_reg#region) sep in
      Buffer.add_string state#buffer sep_line;
      print state item
    in print state head; List.iter print_aux tail

let print_option : state -> (state -> 'a -> unit ) -> 'a option -> unit =
  fun state print -> function
    None -> ()
  | Some opt -> print state opt

let print_csv state print (node : _ Region.reg) =
  print_nsepseq state "," print node.value

let print_token state token _lexeme =
  let line =
    sprintf "%s: %s\n" (compact state token#region) token#payload
  in Buffer.add_string state#buffer line

let print_var state {region; value} =
  let line =
    sprintf "%s: Ident %s\n"
            (compact state region)value
  in Buffer.add_string state#buffer line

let print_constr state {region; value} =
  let line =
    sprintf "%s: Constr %s\n"
            (compact state region)value
  in Buffer.add_string state#buffer line

let print_pconstr state {region; value} =
  let line =
    sprintf "%s: PConstr %s\n"
            (compact state region) value
  in Buffer.add_string state#buffer line

let print_attributes state attributes =
  let apply {value = attribute; region} =
    let attribute_formatted = sprintf "[@%s]" attribute in
    let token = Token.wrap attribute_formatted region in
    print_token state token attribute_formatted
  in List.iter apply attributes

let print_pvar state {region; value} =
  let {variable; attributes} = value in
  let () = print_attributes state attributes in
  let line =
    sprintf "%s: PVar %s\n"
            (compact state region) variable.value
  in Buffer.add_string state#buffer line

let print_string state {region; value} =
  let line =
    sprintf "%s: String %S\n"
            (compact state region) value
  in Buffer.add_string state#buffer line

let print_verbatim state {region; value} =
  let line =
    sprintf "%s: Verbatim %S\n"
            (compact state region) value
  in Buffer.add_string state#buffer line

let print_bytes state {region; value} =
  let lexeme, abstract = value in
  let line =
    sprintf "%s: Bytes (\"%s\", \"0x%s\")\n"
            (compact state region) lexeme
            (Hex.show abstract)
  in Buffer.add_string state#buffer line

let print_int state {region; value} =
  let lexeme, abstract = value in
  let line =
    sprintf "%s: Int (\"%s\", %s)\n"
            (compact state region) lexeme
            (Z.to_string abstract)
  in Buffer.add_string state#buffer line


let rec print_tokens state {statements; eof} =
  Utils.nseq_iter (print_toplevel_statement state) statements;
  print_token state eof "EOF"

and print_toplevel_statement state = function
  TopLevel (statement, terminator) ->
    print_statement  state statement;
    print_terminator state terminator
| Directive dir -> print_directive state dir

and print_directive state dir =
  let s =
    Directive.to_string ~offsets:state#offsets state#mode dir
  in Buffer.add_string state#buffer s

and print_statement state = function
  SBlock {value = { lbrace; inside; rbrace}; _} ->
    print_token   state lbrace "{";
    print_nsepseq state ";" print_statement inside;
    print_token   state rbrace "}";
| SExpr expr -> print_expr state expr
| SCond cond -> print_conditional state cond
| SReturn {value = {kwd_return; expr}; _} ->
    print_token state kwd_return "return";
    print_option state (fun state expr -> print_expr state expr) expr;
| SLet decl ->
    print_let_decl state decl
| SConst decl ->
    print_const_decl state decl
| SType { value = {attributes; kwd_type; name; params; eq; type_expr}; _ } ->
    print_attributes  state attributes;
    print_token       state kwd_type "type";
    print_var         state name;
    print_type_params state params;
    print_token       state eq       "=";
    print_type_expr   state type_expr
| SSwitch {
  value = {
    kwd_switch;
    lpar;
    expr;
    rpar;
    lbrace;
    cases;
    rbrace
  };
  _ } ->
    print_token state kwd_switch "switch";
    print_token state lpar       "(";
    print_expr  state expr;
    print_token state rpar       ")";
    print_token state lbrace    "{";
    print_cases state cases;
    print_token state rbrace    "}"
| SBreak b -> print_token state b "break"
| SNamespace { value = (kwd_namespace, name, {value = {lbrace; inside; rbrace}; _}, attributes); _} ->
    print_attributes state attributes;
    print_token   state kwd_namespace "namespace";
    print_var     state name;
    print_token   state lbrace    "{";
    print_nsepseq state ";" print_statement inside;
    print_token   state rbrace    "}"
| SImport {value = {kwd_import; alias; equal; module_path}; _} ->
    print_token state kwd_import "import";
    print_var   state alias;
    print_token state equal "=";
    print_nsepseq state "." (fun state a -> print_var state a) module_path
| SExport { value = (e, s) ; _} ->
    print_token state e "export";
    print_statement state s
| SForOf stmt ->
    print_for_of state stmt
| SWhile {value = {kwd_while; lpar; expr; rpar; statement} ; _} ->
    print_token state kwd_while "while";
    print_token state lpar "(";
    print_expr state expr;
    print_token state rpar ")";
    print_statement state statement

and print_type_params state (node: type_vars option) =
  match node with
    None -> ()
  | Some {value; _} ->
      let {lchevron; inside; rchevron} = value in
      print_token   state lchevron "<";
      print_nsepseq state "," print_var inside;
      print_token   state rchevron ">"

and print_for_of state (node: for_of reg) =
  let {kwd_for; lpar; index_kind; index;
       kwd_of; expr; rpar; statement} = node.value
  in print_token      state kwd_for "for";
     print_token      state lpar "(";
     print_index_kind state index_kind;
     print_var        state index;
     print_token      state kwd_of "of";
     print_expr       state expr;
     print_token      state rpar ")";
     print_statement  state statement

and print_index_kind state = function
  `Let kwd_let     -> print_token state kwd_let "let"
| `Const kwd_const -> print_token state kwd_const "const"

and print_let_decl state (node: let_decl reg) =
  let {attributes; kwd_let; bindings; _} = node.value in
    print_attributes state attributes;
    print_token      state kwd_let "let";
    print_nsepseq    state "," print_val_binding bindings;

and print_const_decl state (node: const_decl reg) =
  let {attributes; kwd_const; bindings; _} = node.value in
    print_attributes state attributes;
    print_token      state kwd_const "const";
    print_nsepseq    state "," print_val_binding bindings;

and print_type_expr state = function
  TProd prod      -> print_cartesian state prod
| TSum sum        -> print_sum_type state sum
| TObject t       -> print_object_type state t
| TApp app        -> print_type_app state app
| TPar par        -> print_type_par state par
| TVar var        -> print_var state var
| TInt x          -> print_int state x
| TFun t          -> print_fun_type state t
| TString s       -> print_string state s
| TModA ma        -> print_module_access print_type_expr state ma

and print_module_access :
type a.(state -> a -> unit ) -> state -> a module_access reg -> unit =
  fun f state {value; _} ->
  let {module_name; selector; field} = value in
  print_var   state module_name;
  print_token   state selector ".";
  f             state field

and print_sum_type state (node : sum_type reg) =
  let {variants; attributes; leading_vbar} : sum_type = node.value in
  print_attributes state attributes;
  (match leading_vbar with 
    Some leading_vbar ->
      print_token state leading_vbar "|"
  | None -> ());
  print_nsepseq state "|" print_variant variants.value
  
and print_variant state (node : variant reg) =
  let {attributes; tuple} = node.value in
  print_attributes    state attributes;
  print_variant_tuple state tuple

and print_variant_tuple state {value; _} =
  let {lbracket; inside; rbracket} = value in
  print_token        state lbracket "[";
  print_variant_comp state inside;
  print_token        state rbracket "]"

and print_variant_comp state (node : variant_comp) =
  let {constr; params} = node in
  let () = print_var state constr in
  match params with
    None -> ()
  | Some (comma, seq) ->
     (print_token state comma ",";
      print_nsepseq state "," print_type_expr seq)

and print_fun_type_arg state {name; colon; type_expr} =
  print_var       state name;
  print_token     state colon ":";
  print_type_expr state type_expr

and print_fun_type_args state {lpar; inside; rpar} =
  print_token   state lpar "(";
  print_nsepseq state "," print_fun_type_arg inside;
  print_token   state rpar ")";

and print_fun_type state {value; _} =
  let args, arrow, range = value in
  print_fun_type_args state args;
  print_token         state arrow "=>";
  print_type_expr     state range

and print_type_app state {value; _} =
  let value, type_tuple = value in
  print_type_tuple state type_tuple;
  print_var        state value

and print_type_tuple state {value; _} =
  let {lchevron; inside; rchevron} = value in
  print_token   state lchevron "<";
  print_nsepseq state "," print_type_expr inside;
  print_token   state rchevron ">"

and print_type_par state {value={lpar;inside=t;rpar}; _} =
  print_token     state lpar "(";
  print_type_expr state t;
  print_token     state rpar ")"

and print_projection state (node: projection reg) =
  let {expr; selection} = node.value in
  print_expr state expr;
  match selection with
    FieldName { value = {dot; value}; _ } ->
      print_token state dot ".";
      print_var   state value
  | Component { value = {lbracket; inside; rbracket}; _} ->
      print_token state lbracket "[";
      print_expr  state inside;
      print_token state rbracket "]"

and print_cartesian state (node : cartesian) =
  print_attributes state node.attributes;
  let {lbracket; inside; rbracket} = node.inside.value in
  print_token   state lbracket "[";
  print_nsepseq state "," print_type_expr inside;
  print_token   state rbracket "]"

and print_object_type state =
  print_ne_injection state print_field_decl

and print_field_decl state {value; _} =
  let {field_name; colon; field_type; attributes} = value
  in print_attributes state attributes;
     print_var        state field_name;
     print_token      state colon ":";
     print_type_expr  state field_type

and print_ne_injection :
  'a.state -> (state -> 'a -> unit) -> 'a ne_injection reg -> unit =
  fun state print {value; _} ->
  let {compound; ne_elements; terminator; attributes} = value in
    print_attributes     state attributes;
    print_open_compound  state compound;
    print_nsepseq        state ";" print ne_elements;
    print_terminator     state terminator;
    print_close_compound state compound

and print_open_compound state = function
  None -> ()
| Some Braces   (lbrace,  _)  -> print_token state lbrace   "{"
| Some Brackets (lbracket,_)  -> print_token state lbracket "["

and print_close_compound state = function
  None -> ()
| Some Braces   (_, rbrace)   -> print_token state rbrace   "}"
| Some Brackets (_, rbracket) -> print_token state rbracket "]"

and print_terminator state = function
  Some semi -> print_token state semi ";"
| None -> ()

and print_val_binding state (node: val_binding reg) =
  let {binders; lhs_type; eq; expr} = node.value in
  print_pattern state binders;
  print_option state (fun state (colon, type_expr) ->
    print_token state colon ":";
    print_type_expr state type_expr
  ) lhs_type;
  print_token state  eq "=";
  print_expr  state expr

and print_rest_pattern state (node: rest_pattern reg) =
  let {ellipsis; rest} = node.value in
  print_token state ellipsis "...";
  print_var state rest

and print_assign_pattern state (node: assign_pattern reg) =
  let {property; eq; value} = node.value in
  print_var state property;
  print_token state eq "=";
  print_expr state value

and print_destruct_pattern state (node: destruct reg) =
  let {property; colon; target} = node.value in
  print_var state property;
  print_token state colon ":";
  print_val_binding state target

and print_object_pattern state (node: object_pattern) =
  let {lbrace; inside; rbrace} = node.value in
  print_token state lbrace "{";
  print_nsepseq state "," (fun state pattern -> print_pattern state pattern) inside;
  print_token state rbrace "}"

and print_array_pattern state (node: array_pattern) =
  let {lbracket; inside; rbracket} = node.value in
  print_token state lbracket "[";
  print_nsepseq state "," (fun state pattern -> print_pattern state pattern) inside;
  print_token state rbracket "]"

and print_pattern state = function
  PRest e ->     print_rest_pattern     state e
| PAssign e ->   print_assign_pattern   state e
| PVar v ->      print_pvar             state v
| PConstr v ->   print_pconstr          state v
| PDestruct d -> print_destruct_pattern state d
| PObject o ->   print_object_pattern   state o
| PArray a ->    print_array_pattern    state a

and print_property state = function
  Punned_property {value; _} -> print_expr state value
| Property {value = {name; colon; value}; _ } ->
    print_expr state name;
    print_token state colon ":";
    print_expr state value;
| Property_rest {value = {ellipsis; expr}; _} ->
    print_token state ellipsis "...";
    print_expr state expr

and print_object state (node: object_expr) =
  let {lbrace; inside; rbrace} = node.value in
  print_token state lbrace "{";
  print_nsepseq state "," (fun state property -> print_property state property) inside;
  print_token state rbrace "}"

and print_assignment state (lhs, op, rhs) =
  print_expr state lhs;
  let lexeme = match op.value with 
    Eq -> " = "
  | Assignment_operator Times_eq ->  " *= "  
  | Assignment_operator Div_eq ->    " /= "
  | Assignment_operator Min_eq ->    " -= "
  | Assignment_operator Plus_eq ->   " += "
  | Assignment_operator Mod_eq ->    " %= "
  in
  let token = Token.wrap lexeme op.region in
  print_token state token lexeme;
  print_expr state rhs;

and print_expr state = function
  EFun e                 -> print_fun_expr    state e
| EPar e                 -> print_expr_par    state e
| ESeq seq               -> print_sequence    state seq
| EVar v                 -> print_var       state v
| EModA ma               -> print_module_access print_expr state ma
| EAssign (lhs, eq, rhs) -> print_assignment  state (lhs, eq, rhs)
| ELogic e               -> print_logic_expr  state e
| EArith e               -> print_arith_expr  state e
| ECall e                -> print_fun_call    state e
| EBytes e               -> print_bytes       state e
| EArray e               -> print_array       state e
| EObject e              -> print_object      state e
| EString e              -> print_string_expr state e
| EProj e                -> print_projection  state e
| EAnnot e               -> print_annot_expr  state e
| EUnit e                -> print_unit        state e
| EConstr e              -> print_constr_expr state e
| ECodeInj e             -> print_code_inj    state e

and print_constr_expr state {value; _} =
  let constr, argument = value in
  print_constr state constr;
  match argument with
    None -> ()
  | Some arg -> print_expr state arg

and print_array_item state = function
  Expr_entry expr -> print_expr state expr
| Rest_entry {value = {ellipsis; expr}; _} ->
  print_token state ellipsis "...";
  print_expr state expr

and print_array state {value = {lbracket; inside; rbracket};_ } =
  print_token state lbracket "[";
  (match inside with 
    Some inside -> 
      print_nsepseq state "," print_array_item inside;
  | None -> ());
  print_token state rbracket "]"

and print_expr_par state {value; _} =
  let {lpar;inside=e;rpar} = value in
  print_token state lpar "(";
  print_expr  state e;
  print_token state rpar ")"

and print_unit state {value=lpar,rpar; _} =
  print_token state lpar "(";
  print_token state rpar ")"

and print_fun_call state {value=f,l; _} =
  print_expr state f;
  match l with
  | Unit e -> print_unit state e
  | Multiple {value={lpar;inside;rpar};region}->
     print_token state lpar "(";
     print_csv state print_expr {value=inside;region};
     print_token state rpar ")"

and print_annot_expr state {value; _} =
  let (e,colon,t) = value in
  print_expr  state e;
  print_token state colon "as";
  print_type_expr state t;

and print_arith_expr state = function
  Add {value={arg1;op;arg2}; _} ->
    print_expr  state arg1;
    print_token state op "+";
    print_expr  state arg2
| Sub {value={arg1;op;arg2}; _} ->
    print_expr  state arg1;
    print_token state op "-";
    print_expr  state arg2
| Mult {value={arg1;op;arg2}; _} ->
    print_expr  state arg1;
    print_token state op "*";
    print_expr  state arg2
| Div {value={arg1;op;arg2}; _} ->
    print_expr  state arg1;
    print_token state op "/";
    print_expr  state arg2
| Mod {value={arg1;op;arg2}; _} ->
    print_expr  state arg1;
    print_token state op "%";
    print_expr  state arg2
| Neg {value={op;arg}; _} ->
    print_token state op "-";
    print_expr  state arg
| Int {region; value=lex,z} ->
    let line = sprintf "Int %s (%s)" lex (Z.to_string z) in 
    let token = Token.wrap line region in
    print_token state token line

and print_string_expr state = function
  String s ->
    print_string state s
| Verbatim v ->
    print_verbatim state v

and print_logic_expr state = function
  BoolExpr e -> print_bool_expr state e
| CompExpr e -> print_comp_expr state e

and print_bool_expr state = function
  Or {value={arg1;op;arg2}; _} ->
    print_expr  state arg1;
    print_token state op "||";
    print_expr  state arg2
| And {value={arg1;op;arg2}; _} ->
    print_expr  state arg1;
    print_token state op "&&";
    print_expr  state arg2
| Not {value={op;arg}; _} ->
    print_token state op "!";
    print_expr  state arg

and print_comp_expr state = function
  Lt {value={arg1;op;arg2}; _} ->
    print_expr  state arg1;
    print_token state op "<";
    print_expr  state arg2
| Leq {value={arg1;op;arg2}; _} ->
    print_expr  state arg1;
    print_token state op "<=";
    print_expr  state arg2
| Gt {value={arg1;op;arg2}; _} ->
    print_expr  state arg1;
    print_token state op ">";
    print_expr  state arg2
| Geq {value={arg1;op;arg2}; _} ->
    print_expr  state arg1;
    print_token state op ">=";
    print_expr  state arg2
| Neq {value={arg1;op;arg2}; _} ->
    print_expr  state arg1;
    print_token state op "<>";
    print_expr  state arg2
| Equal {value={arg1;op;arg2}; _} ->
    print_expr  state arg1;
    print_token state op "=";
    print_expr  state arg2

and print_code_inj state {value; _} =
  let {language; code} = value in
  (* let header_stop = region#start#shift_bytes 1 in *)
  (* let header_reg  = Region.make ~start:region#start ~stop:header_stop in *)
  (* print_token  state lbacktick "`"; *)
  print_string state language;
  print_expr   state code;
  (* print_token  state rbracket "`" *)

and print_sequence state {value; _} =
  print_nsepseq state "," print_expr value

and print_cases state value =
  let apply len rank =
    print_case_clause (state#pad len rank) in
  let decls = Utils.nseq_to_list value in
  List.iteri (List.length decls |> apply) decls

and print_case_clause state = function
  Switch_case { kwd_case; expr; colon; statements } ->
    print_token  state kwd_case "case";
    print_expr   state expr;
    print_token  state colon ":";
    print_option state (fun state statements ->
      print_nsepseq state ";" print_statement statements
    ) statements
| Switch_default_case { kwd_default; colon; statements } ->
  print_token  state kwd_default "default";
  print_token  state colon ":";
  print_option state (fun state statements ->
    print_nsepseq state ";" print_statement statements
  ) statements

and print_fun_expr state {value; _} =
  let { parameters; lhs_type; arrow; body } = value in
  print_expr   state parameters;
  print_option state (fun state (colon, type_expr) ->
    print_token state colon ":";
    print_type_expr state type_expr
  ) lhs_type;
  print_token state arrow "=>";
  match body with
    FunctionBody { value = {lbrace; inside; rbrace}; _ } ->
      print_token state lbrace "{";
      print_nsepseq state ";" print_statement inside;
      print_token state rbrace "}"
  | ExpressionBody expr ->
      print_expr state expr

and print_conditional state {value; _} =
  let {kwd_if; test = {lpar; inside; rpar}; ifso; ifnot} = value in
  print_token state ghost "(";
  print_token state kwd_if "if";
  print_token state lpar "(";
  print_expr state inside;
  print_token state rpar ")";
  print_statement state ifso;
  print_option state
    (fun state (kwd_else,ifnot) ->
      print_token state kwd_else "else";
      print_statement state ifnot
    ) ifnot;
  print_token state ghost ")"

(* Conversion to string *)

let to_string ~offsets ~mode printer node =
  let buffer = Buffer.create 131 in
  let state = mk_state ~offsets ~mode ~buffer in
  let () = printer state node
  in Buffer.contents buffer

let tokens_to_string ~offsets ~mode =
  to_string ~offsets ~mode print_tokens
(* let pattern_to_string ~offsets ~mode =
  to_string ~offsets ~mode print_pattern *)
let expr_to_string ~offsets ~mode =
  to_string ~offsets ~mode print_expr
let type_expr_to_string ~offsets ~mode =
  to_string ~offsets ~mode print_type_expr

(** {1 Pretty-printing the AST} *)

let pp_ident state {value=name; region} =
  let reg  = compact state region in
  let node = sprintf "%s%s (%s)\n" state#pad_path name reg
  in Buffer.add_string state#buffer node

let pp_node state name =
  let node = sprintf "%s%s\n" state#pad_path name
  in Buffer.add_string state#buffer node

let pp_string state {value=name; region} =
  let reg  = compact state region in
  let node = sprintf "%s%S (%s)\n" state#pad_path name reg
  in Buffer.add_string state#buffer node

(* TODO *)
let _pp_verbatim state {value=name; region} =
  let reg  = compact state region in
  let node = sprintf "%s{|%s|} (%s)\n" state#pad_path name reg
  in Buffer.add_string state#buffer node

let pp_loc_node state name region =
  pp_ident state {value=name; region}

let rec pp_cst state {statements; _} =
  let statements = Utils.nseq_to_list statements in
  let apply len rank = pp_toplevel_statement (state#pad len rank)
  in pp_node state "<ast>";
     List.iteri (List.length statements |> apply) statements

and pp_toplevel_statement state = function
  TopLevel (stmt, _) -> pp_statement state stmt
| Directive dir ->
    let region, string = Directive.project dir in
    pp_loc_node state "Directive" region;
    pp_node state string

and pp_statement state = function
  SBlock {value = {inside;_}; region} ->
    pp_loc_node state "SBlock" region;
    let statements = Utils.nsepseq_to_list inside in
    let apply len rank = pp_statement (state#pad len rank) in
    List.iteri (List.length statements |> apply) statements
| SExpr e ->
    pp_node  state "SExpr";
    pp_expr (state#pad 1 0) e
| SCond {value; region} ->
    pp_loc_node state "SCond" region;
    pp_cond_statement state value
| SReturn {value = {expr; _}; region} -> (
    pp_loc_node state "SReturn" region;
    match expr with
    | Some e -> pp_expr (state#pad 1 0) e
    | None -> ()
)
| SLet stmt ->
    pp_let_stmt state stmt
| SConst stmt ->
    pp_const_stmt state stmt
| SType {value; region} ->
    pp_loc_node state "SType" region;
    pp_type_decl state value
| SSwitch {value; region} ->
    pp_loc_node state "SSwitch" region;
    pp_switch_statement state value
| SBreak kwd_break ->
    pp_loc_node state "SBreak" kwd_break#region
| SNamespace {value; region} ->
    pp_loc_node  state "SNamespace" region;
    pp_namespace state value
| SExport {value; region} ->
    pp_loc_node state "SExport" region;
    pp_statement state (snd value)
| SImport {value; region} ->
    pp_loc_node state "SImport" region;
    pp_import state value
| SForOf {value; region} ->
    pp_loc_node state "SForOf" region;
    pp_for_of state value
| SWhile {value; region} ->
    pp_loc_node state "SWhile" region;
    pp_while state value

and pp_let_stmt state (node: let_decl reg) =
  let {attributes; bindings; _} : let_decl = node.value in
  let val_bindings = Utils.nsepseq_to_list bindings in
  (if attributes <> [] then
    pp_attributes state attributes);
  pp_loc_node state "SLet" node.region;
  let len = List.length val_bindings in
  let apply rank = pp_val_binding (state#pad len rank) in
  List.iteri apply val_bindings

and pp_const_stmt state (node: const_decl reg) =
  let {attributes; bindings; _} : const_decl = node.value in
  let val_bindings = Utils.nsepseq_to_list bindings in
  (if attributes <> [] then
    pp_attributes state attributes);
  pp_loc_node state "SConst" node.region;
  let apply len rank = pp_val_binding (state#pad len rank) in
  List.iteri (List.length val_bindings |> apply) val_bindings

and pp_for_of state {index; expr; statement; _} =
  pp_ident state index;
  pp_expr state expr;
  pp_statement state statement

and pp_while state {expr; statement; _} =
  pp_expr state expr;
  pp_statement state statement

and pp_import state  {alias; module_path; _} =
  pp_ident state alias;
  let items = Utils.nsepseq_to_list module_path in
  let aux p = pp_ident state p in
  List.iter aux items

and pp_namespace state (n, name, {value = {inside = statements;_}; _}, attributes) =
  pp_loc_node state "<namespace>" n#region;
  pp_attributes state attributes;
  pp_ident    state name;
  let statements = Utils.nsepseq_to_list statements in
  let apply len rank = pp_statement (state#pad len rank) in
  List.iteri (List.length statements |> apply) statements


and pp_switch_statement state node =
  let {expr; cases; _} = node in
  pp_node state "<expr>";
  pp_expr (state#pad 1 0) expr;
  let cases = Utils.nseq_to_list cases in
  let length = List.length cases + 1 in
  let apply len rank = pp_case (state#pad len (rank+1))
  in List.iteri (apply length) cases

and pp_case state = function
  Switch_case { expr; statements; _ } ->
    pp_node state "<case>";
    pp_expr state expr;
    (match statements with
    | Some statements ->
      let statements = Utils.nsepseq_to_list statements in
      let apply len rank = pp_statement (state#pad len rank) in
      List.iteri (List.length statements |> apply) statements
    | None -> ())
| Switch_default_case { statements; _ } ->
    pp_node state "<default>";
    (match statements with
    | Some statements ->
      let statements = Utils.nsepseq_to_list statements in
      let apply len rank = pp_statement (state#pad len rank) in
      List.iteri (List.length statements |> apply) statements
    | None -> ())

and pp_val_binding state (node: val_binding reg) =
  let {binders; lhs_type; expr; _} = node.value in
  let fields = if lhs_type = None then 2 else 3 in
  let arity = 0 in
  pp_node state "<binding>";
  pp_pattern (state#pad fields arity) binders;
  let arity = match lhs_type with
  | Some (_, type_expr) ->
    let state = state#pad fields (arity + 1) in
    pp_node state "<lhs type>";
    pp_type_expr (state#pad 1 0) type_expr;
    arity + 1
  | None -> arity in
  let state = state#pad fields (arity + 1) in
  pp_node state "<expr>";
  pp_expr (state#pad 1 0) expr

and pp_pvar state (node: var_pattern reg) =
  let {variable; attributes} = node.value in
  if attributes = [] then
    pp_ident state variable
  else
    (pp_node       state "PVar";
     pp_ident      (state#pad 2 0) variable;
     pp_attributes (state#pad 2 1) attributes)

and pp_pattern state = function
  PRest { value = {rest; _}; region} ->
    pp_loc_node state "<rest>" region;
    pp_ident (state#pad 1 0) rest
| PAssign { value = {property; value; _}; region} ->
    pp_loc_node state "<assign>" region;
    pp_ident (state#pad 1 0) property;
    pp_expr  (state#pad 1 0) value
| PVar v -> pp_pvar state v
| PConstr v ->
    pp_node state "<constr>";
    pp_ident (state#pad 1 0) v
| PDestruct {value = {property; target; _}; region} ->
    pp_loc_node state "<destruct>" region;
    pp_ident (state#pad 1 0) property;
    pp_val_binding state target
| PObject {value = {inside; _}; region} ->
    pp_loc_node state "<object>" region;
    let properties = Utils.nsepseq_to_list inside in
    let apply len rank = pp_pattern (state#pad len rank) in
    List.iteri (List.length properties |> apply) properties
| PArray {value = {inside; _}; region} ->
    pp_loc_node state "<array>" region;
    let items = Utils.nsepseq_to_list inside in
    let apply len rank = pp_pattern (state#pad len rank) in
    List.iteri (List.length items |> apply) items

and pp_type_decl state decl =
  pp_ident     (state#pad 2 0) decl.name;
  pp_type_expr (state#pad 2 1) decl.type_expr

and pp_ne_injection :
  'a.(state -> 'a -> unit) -> state -> 'a ne_injection -> unit =
  fun printer state inj ->
    let ne_elements    = Utils.nsepseq_to_list inj.ne_elements in
    let length         = List.length ne_elements in
    let arity          = if inj.attributes = [] then length else length + 1
    and apply len rank = printer (state#pad len rank)
    in List.iteri (apply arity) ne_elements;
       let state = state#pad arity (arity-1)
       in
       if inj.attributes <> [] then
        pp_attributes state inj.attributes

and pp_bytes state {value=lexeme,hex; region} =
  pp_loc_node (state#pad 2 0) lexeme region;
  pp_node     (state#pad 2 1) (Hex.show hex)

and pp_int state {value=lexeme,z; region} =
  pp_loc_node (state#pad 2 0) lexeme region;
  pp_node     (state#pad 2 1) (Z.to_string z)

and pp_expr state = function
  EFun {value; region} ->
    pp_loc_node state "EFun" region;
    pp_fun_expr state value
| EPar {value = {inside; _}; region} ->
    pp_loc_node state "EPar" region;
    pp_expr (state#pad 1 0) inside
| ESeq {value; region} ->
    pp_loc_node state "ESeq" region;
    let exprs = Utils.nsepseq_to_list value in
    let apply len rank = pp_expr (state#pad len rank) in
    List.iteri (List.length exprs |> apply) exprs
| EAssign (lhs, _, rhs) ->
    pp_node state "EAssign";
    pp_expr (state#pad 1 0) lhs;
    pp_expr (state#pad 1 0) rhs
| EVar v ->
    pp_node  state "EVar";
    pp_ident (state#pad 1 0) v
| ELogic e_logic ->
    pp_node state "ELogic";
    pp_e_logic (state#pad 1 0) e_logic
| EArith e_arith ->
    pp_node state "EArith";
    pp_arith_expr (state#pad 1 0) e_arith
| ECall {value; region} ->
    pp_loc_node state "ECall" region;
    pp_fun_call (state#pad 1 0) value
| EBytes b ->
    pp_node state "EBytes";
    pp_bytes state b
| EArray {value = {inside; _}; region} ->
    pp_loc_node state "EArray" region;
    (match inside with 
      Some inside ->
        let items  = Utils.nsepseq_to_list inside in
        let apply len rank = pp_array_item (state#pad len rank) in
        List.iteri (List.length items |> apply) items
    | None -> 
        pp_loc_node state "<empty>" region)
| EConstr e_constr ->
    pp_node state "EConstr";
    pp_constr_expr (state#pad 1 0) e_constr
| EObject {value = {inside; _}; region} ->
    pp_loc_node state "EObject" region;
    let properties  = Utils.nsepseq_to_list inside in
    let apply len rank = pp_property (state#pad len rank) in
    List.iteri (List.length properties |> apply) properties
| EString e_string ->
    pp_node state "EString";
    pp_string_expr (state#pad 1 0) e_string
| EProj {value = {expr; selection}; region} ->
    pp_loc_node state "EProj" region;
    (* let state = state#pad 2 0 in *)
    pp_expr (state#pad 2 0) expr;
    (match selection with
      FieldName {value = {value; _}; region} ->
        let state = state#pad 2 1 in
        pp_loc_node state "<fieldname>" region;
        pp_ident (state#pad 1 0) value
    | Component {value = {inside; _}; region} ->
        let state = state#pad 2 1 in
        pp_loc_node state "<component>" region;
        pp_expr (state#pad 1 0) inside)
| EModA {value; region} ->
  pp_loc_node state "EModA" region;
  pp_module_access pp_expr state value
| EAnnot {value; region} ->
    pp_loc_node state "EAnnot" region;
    pp_annotated state value
| EUnit {region; _} ->
    pp_loc_node state "EUnit" region;
| ECodeInj {value; region} ->
    pp_loc_node state "ECodeInj" region;
    pp_code_inj state value

and pp_constr_expr state (node: (constr * expr option) reg) =
  let constr, expr_opt = node.value in
  pp_loc_node state "EConstr" node.region;
  match expr_opt with
    None -> pp_ident (state#pad 1 0) constr
  | Some expr ->
     pp_ident (state#pad 2 0) constr;
     pp_expr  (state#pad 2 1) expr

and pp_array_item state = function
  Expr_entry e ->
    pp_node state "<expr>";
    pp_expr (state#pad 1 0) e
| Rest_entry {value; region} ->
    pp_loc_node state "<rest>" region;
    pp_expr    (state#pad 1 0) value.expr

and pp_property state = function
  Punned_property {value; region} ->
    pp_loc_node state "<punned property>" region;
    pp_expr state value
| Property { value = {name; value; _}; region } ->
    pp_loc_node state "<property>" region;
    pp_expr (state#pad 2 0) name;
    pp_expr (state#pad 2 1) value
| Property_rest {value = {expr; _}; region} ->
    pp_loc_node state "<property rest>" region;
    pp_expr state expr

and pp_fun_expr state node =
  let {parameters; lhs_type; body; _} = node in
  let fields = if lhs_type = None then 2 else 3 in
  let () =
    let state = state#pad fields 0 in
    pp_node state "<parameters>";
    pp_expr (state#pad 1 0) parameters in
  let () =
    match lhs_type with
      None -> ()
    | Some (_, type_expr) ->
       let state = state#pad fields 1 in
       pp_node state "<lhs type>";
       pp_type_expr (state#pad 1 0) type_expr in
  let () =
    let state = state#pad fields (fields - 1) in
    (match body with
      FunctionBody {value = {inside;_}; region} ->
        let statements = Utils.nsepseq_to_list inside in
        let apply len rank = pp_statement (state#pad len rank) in

        pp_loc_node state "<function_body>" region;
        List.iteri (List.length statements |> apply) statements
    | ExpressionBody e_body ->
        pp_node state "<expression body>";
        pp_expr (state#pad 1 0) e_body
    )
  in ()

and pp_code_inj state rc =
  let () =
    let state = state#pad 2 0 in
    pp_node state "<language>";
    pp_string (state#pad 1 0) rc.language in
  let () =
    let state = state#pad 2 1 in
    pp_node state "<code>";
    pp_expr (state#pad 1 0) rc.code
  in ()

and pp_tuple_expr state (node: (expr, comma) Utils.nsepseq reg) =
  let exprs          = Utils.nsepseq_to_list node.value in
  let length         = List.length exprs in
  let apply len rank = pp_expr (state#pad len rank)
  in List.iteri (apply length) exprs

and pp_arguments state = function
  | Multiple x ->
     let ({inside; _}: (expr,comma) Utils.nsepseq par) = x.value in
     pp_tuple_expr state {value=inside; region = x.region};
  | Unit x ->
     print_unit state x

and pp_fun_call state (fun_expr, args) =
  let arity = match args with
    | Unit _ -> 0
    | Multiple xs -> List.length (Utils.nsepseq_to_list xs.value.inside) in
  pp_expr (state#pad (1+arity) 0) fun_expr;
  pp_arguments state args

and pp_string_expr state = function
  String s ->
    pp_node   state "String";
    pp_string (state#pad 1 0) s
| Verbatim v ->
    pp_node   state "Verbatim";
    pp_string (state#pad 1 0) v

and pp_arith_expr state = function
  Add {value; region} ->
    pp_bin_op "Add" region state value
| Sub {value; region} ->
    pp_bin_op "Sub" region state value
| Mult {value; region} ->
    pp_bin_op "Mult" region state value
| Div {value; region} ->
    pp_bin_op "Div" region state value
| Mod {value; region} ->
    pp_bin_op "Mod" region state value
| Neg {value; region} ->
    pp_loc_node state "Neg" region;
    pp_expr (state#pad 1 0) value.arg;
| Int i ->
    pp_node state "Int";
    pp_int  state i

and pp_e_logic state = function
  BoolExpr e ->
    pp_node state "BoolExpr";
    pp_bool_expr (state#pad 1 0) e
| CompExpr e ->
    pp_node state "CompExpr";
    pp_comp_expr (state#pad 1 0) e

and pp_bool_expr state = function
  Or {value; region} ->
    pp_bin_op "Or" region state value
| And {value; region} ->
    pp_bin_op "And" region state value
| Not {value; _} ->
    pp_node state "Not";
    pp_expr (state#pad 1 0) value.arg

and pp_comp_expr state = function
  Lt {value; region} ->
    pp_bin_op "Lt" region state value
| Leq {value; region} ->
    pp_bin_op "Leq" region state value
| Gt {value; region} ->
    pp_bin_op "Gt" region state value
| Geq {value; region} ->
    pp_bin_op "Geq" region state value
| Equal {value; region} ->
    pp_bin_op "Equal" region state value
| Neq {value; region} ->
    pp_bin_op "Neq" region state value

and pp_bin_op node region state op =
  pp_loc_node state node region;
  pp_expr (state#pad 2 0) op.arg1;
  pp_expr (state#pad 2 1) op.arg2

and pp_annotated state annot =
  let expr, _, t_expr = annot in
  pp_expr      (state#pad 2 0) expr;
  pp_type_expr (state#pad 2 1) t_expr

and pp_cond_statement state (cond: cond_statement) =
  let () =
    let state = state#pad 3 0 in
    pp_node state "<condition>";
    pp_expr (state#pad 1 0) cond.test.inside in
  let () =
    let state = state#pad 3 1 in
    pp_node state "<true>";
    pp_statement (state#pad 1 0) cond.ifso in
  let () = match cond.ifnot with
    Some ifnot ->
    let state = state#pad 3 2 in
    pp_node state "<false>";
    pp_statement (state#pad 1 0) @@ (snd ifnot)
  | None -> ()
  in ()

and pp_type_expr state = function
  TProd {inside; attributes} ->
    let {value; region} = inside in
    pp_attributes state attributes;
    pp_loc_node state "TProd" region;
    pp_cartesian state value
| TSum {value; region} ->
    pp_loc_node state "TSum" region;
    pp_sum_type state value
| TObject {value; region} ->
    pp_loc_node state "TObject" region;
    pp_ne_injection pp_field_decl state value
| TApp {value=name,tuple; region} ->
    pp_loc_node   state "TApp" region;
    pp_ident      (state#pad 1 0) name;
    pp_type_tuple (state#pad 2 1) tuple
| TFun {value; region} ->
    pp_loc_node state "TFun" region;
    let state = state#pad 0 1 in
    let apply len rank =
      pp_type_expr (state#pad len rank) in
    let args, _, range = value in
    pp_fun_type_args state args;
    pp_loc_node state "<result>" region;
    List.iteri (apply 2) [range]
| TPar {value={inside;_}; region} ->
    pp_loc_node  state "TPar" region;
    pp_type_expr (state#pad 1 0) inside
| TVar v ->
    pp_node  state "TVar";
    pp_ident (state#pad 1 0) v
| TString s ->
    pp_node   state "TString";
    pp_string (state#pad 1 0) s
| TModA {value; region} ->
    pp_loc_node state "TModA" region;
    pp_module_access pp_type_expr state value
| TInt s ->
    pp_node   state "TInt";
    pp_int (state#pad 1 0) s

and pp_module_access : type a. (state -> a -> unit ) -> state -> a module_access -> unit
= fun f state ma ->
  pp_ident (state#pad 2 0) ma.module_name;
  f (state#pad 2 1) ma.field

and pp_fun_type_arg state (node: fun_type_arg) =
  let {name; type_expr; _} : fun_type_arg = node in
  pp_ident     state name;
  let state = (state#pad 1 0) in
  pp_type_expr state type_expr

and pp_fun_type_args state {inside; _} =
  pp_node state "<parameters>";
  let fun_type_args = Utils.nsepseq_to_list inside in
  let apply len rank = pp_fun_type_arg (state#pad len rank) in
  List.iteri (List.length fun_type_args |> apply) fun_type_args

and pp_sum_type state {variants; attributes; _} =
  let variants = Utils.nsepseq_to_list variants.value in
  let arity    = List.length variants in
  let arity    = if attributes = [] then arity else arity+1 in
  let apply arity rank variant =
    let state = state#pad arity rank
    in pp_variant state variant in
  let () = List.iteri (apply arity) variants in
  if attributes <> [] then
    let state = state#pad arity (arity-1)
    in pp_attributes state attributes

and pp_variant state (node : variant reg) =
  let {attributes; tuple; _} = node.value in
  let arity = if attributes = [] then 0 else 1 in
  let {constr; params} = tuple.value.inside in
  let params =
    match params with
      None -> []
    | Some (_, seq) -> Utils.nsepseq_to_list seq in
  let arity = if params = [] then arity else arity+1 in
  let rank = 0 in
  let () = pp_ident state constr in
  let rank =
    match params with
      [] -> rank
    | components ->
        let apply len rank = pp_type_expr (state#pad len rank)
        in List.iteri (List.length components |> apply) components; rank+1 in
  let () = if attributes <> [] then
             pp_attributes (state#pad arity rank) attributes
  in ()

and pp_type_tuple state {value; _} =
  let components     = Utils.nsepseq_to_list value.inside in
  let apply len rank = pp_type_expr (state#pad len rank)
  in List.iteri (List.length components |> apply) components

and pp_attributes state attributes =
  pp_node state "<attributes>";
  let length         = List.length attributes in
  let apply len rank = pp_ident (state#pad len rank)
  in List.iteri (apply length) attributes

and pp_field_decl state {value; _} =
  let arity = if value.attributes = [] then 1 else 2 in
  pp_ident      state value.field_name;
  pp_type_expr  (state#pad arity 0) value.field_type;
  if value.attributes <> [] then
    pp_attributes (state#pad arity 1) value.attributes

and pp_cartesian state {inside;_} =
  let t_exprs        = Utils.nsepseq_to_list inside in
  let arity          = List.length t_exprs in
  let apply len rank = pp_type_expr (state#pad len rank)
  in List.iteri (apply arity) t_exprs;
