[@@@warning "-42"]
[@@@coverage exclude_file]

open CST

module Directive = LexerLib.Directive
module Region = Simple_utils.Region
open! Region

let sprintf = Printf.sprintf

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

    (* The method [pad] updates the current padding, which is
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

(* Printing the tokens with their source regions *)

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

let print_sepseq :
  state -> string -> (state -> 'a -> unit) ->
  ('a, _ Token.wrap) Utils.sepseq -> unit =
  fun state sep print -> function
        None -> ()
  | Some seq -> print_nsepseq state sep print seq

let print_option : state -> (state -> 'a -> unit) -> 'a option -> unit =
  fun state print -> function
    None -> ()
  | Some opt -> print state opt

let print_token state token _lexeme =
  let line =
    sprintf "%s: %s\n"(compact state token#region) token#payload
  in Buffer.add_string state#buffer line

let print_var state {region; value} =
  let line =
    sprintf "%s: Ident %S\n"
            (compact state region) value
  in Buffer.add_string state#buffer line

let print_constr state {region; value} =
  let line =
    sprintf "%s: Constr %S\n"
            (compact state region) value
  in Buffer.add_string state#buffer line

let print_string state {region; value} =
  let line =
    sprintf "%s: String %S\n"
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

let print_verbatim state {region; value} =
  let line =
    sprintf "%s: Verbatim %S\n"
            (compact state region) value
  in Buffer.add_string state#buffer line

let print_bytes state {region; value} =
  let lexeme, abstract = value in
  let line =
    sprintf "%s: Bytes (%S, \"0x%s\")\n"
            (compact state region) lexeme
            (Hex.show abstract)
  in Buffer.add_string state#buffer line

let print_int state {region; value} =
  let lexeme, abstract = value in
  let line =
    sprintf "%s: Int (%S, %s)\n"
            (compact state region) lexeme
            (Z.to_string abstract)
  in Buffer.add_string state#buffer line

let print_nat state {region; value} =
  let lexeme, abstract = value in
  let line =
    sprintf "%s: Nat (%S, %s)\n"
            (compact state region) lexeme
            (Z.to_string abstract)
  in Buffer.add_string state#buffer line

let rec print_tokens state ast =
  let {decl; eof} = ast in
  Utils.nseq_iter (print_decl state) decl;
  print_token state eof "EOF"

and print_decl state = function
  TypeDecl    decl -> print_type_decl    state decl
| ConstDecl   decl -> print_const_decl   state decl
| FunDecl     decl -> print_fun_decl     state decl
| ModuleDecl  decl -> print_module_decl  state decl
| ModuleAlias decl -> print_module_alias state decl
| Directive   dir  -> print_directive    state dir

and print_directive state dir =
  let s =
    Directive.to_string ~offsets:state#offsets state#mode dir
  in Buffer.add_string state#buffer s

and print_const_decl state {value; _} =
  let {kwd_const; pattern; const_type;
       equal; init; terminator; attributes; _} = value in
  print_attributes state attributes;
  print_token      state kwd_const "const";
  print_pattern    state pattern;
  print_option     state print_type_annot const_type;
  print_token      state equal "=";
  print_expr       state init;
  print_terminator state terminator

and print_type_decl state {value; _} =
  let {kwd_type; name; params; kwd_is;
       type_expr; terminator;_ } = value in
  print_token      state kwd_type "type";
  print_var        state name;
  print_option     state print_type_vars params;
  print_token      state kwd_is "is";
  print_type_expr  state type_expr;
  print_terminator state terminator

and print_type_vars state (node : type_vars) =
  let {inside; _} : _ par = node.value in
  print_nsepseq state "," print_type_var inside

and print_type_var state = print_var state

and print_module_decl state {value; _} =
  let {kwd_module; name; kwd_is;enclosing;
       module_; terminator} = value in
  match enclosing with
    Brace (lbrace, rbrace) ->
    print_token      state kwd_module "module";
    print_var        state name;
    print_token      state kwd_is "is";
    print_token      state lbrace "{";
    print_tokens     state module_;
    print_token      state rbrace "}";
    print_terminator state terminator
  | BeginEnd (kwd_begin, kwd_end) ->
    print_token      state kwd_module "module";
    print_var        state name;
    print_token      state kwd_is "is";
    print_token      state kwd_begin "begin";
    print_tokens     state module_;
    print_token      state kwd_end "end";
    print_terminator state terminator

and print_module_alias state {value; _} =
  let {kwd_module; alias; kwd_is; binders;terminator} = value in
    print_token      state kwd_module "module";
    print_var        state alias;
    print_token      state kwd_is "is";
    print_nsepseq    state "." print_var binders;
    print_terminator state terminator

and print_type_expr state = function
  TProd   cartesian   -> print_cartesian   state cartesian
| TSum    sum_type    -> print_sum_type    state sum_type
| TRecord record_type -> print_record_type state record_type
| TApp    type_app    -> print_type_app    state type_app
| TFun    type_fun    -> print_type_fun    state type_fun
| TPar    par_type    -> print_par_type    state par_type
| TVar    type_var    -> print_var         state type_var
| TString str         -> print_string      state str
| TInt    i           -> print_int         state i
| TModA   ma          -> print_module_access print_type_expr state ma

and print_type_annot state (colon, type_expr) =
  print_token     state colon ":";
  print_type_expr state type_expr;

and print_cartesian state {value; _} =
  print_nsepseq state "*" print_type_expr value

and print_variant state {value; _} =
  let {constr; arg; attributes=attr} = value in
  print_attributes state attr;
  print_constr state constr;
  match arg with
    None -> ()
  | Some (kwd_of, t_expr) ->
      print_token     state kwd_of "of";
      print_type_expr state t_expr

and print_sum_type state {value; _} =
  let {variants; attributes; lead_vbar} = value in
  print_attributes state attributes;
  print_token_opt  state lead_vbar "|";
  print_nsepseq    state "|" print_variant variants

and print_record_type state =
  print_ne_injection state print_field_decl

and print_type_app state {value; _} =
  let type_name, type_tuple = value in
  print_var        state type_name;
  print_type_tuple state type_tuple

and print_type_fun state {value; _} =
  let type_expr_a, arrow, type_expr_b = value in
  print_type_expr state type_expr_a;
  print_token     state arrow "->";
  print_type_expr state type_expr_b

and print_par_type state {value; _} =
  let {lpar; inside; rpar} = value in
  print_token     state lpar "(";
  print_type_expr state inside;
  print_token     state rpar ")"

and print_field_decl state {value; _} =
  let {field_name; colon; field_type; attributes} = value
  in print_attributes state attributes;
     print_var        state field_name;
     print_token      state colon ":";
     print_type_expr  state field_type

and print_type_tuple state {value; _} =
  let {lpar; inside; rpar} = value in
  print_token state lpar "(";
  print_nsepseq state "," print_type_expr inside;
  print_token state rpar ")"

and print_fun_decl state {value; _} =
  let {kwd_function; fun_name; param;
       ret_type; kwd_is;
       return; terminator; attributes; _} = value in
  print_attributes state attributes;
  print_token      state kwd_function "function";
  print_var        state fun_name;
  print_parameters state param;
  print_option     state print_type_annot ret_type;
  print_token      state kwd_is "is";
  print_expr state return;
  print_terminator state terminator;

and print_fun_expr state {value; _} =
  let {attributes; kwd_function; param;
       ret_type; kwd_is; return} : fun_expr = value in
  print_attributes state attributes;
  print_token      state kwd_function "function";
  print_parameters state param;
  print_option     state print_type_annot ret_type;
  print_token      state kwd_is "is";
  print_expr       state return

and print_code_inj state {value; _} =
  let {language; code; rbracket} = value in
  let {value=lang; region} = language in
  let header_stop = region#start#shift_bytes 1 in
  let header_reg  = Region.make ~start:region#start ~stop:header_stop in
  let header_t = Token.wrap "[%" header_reg in
  print_token  state header_t "[%";
  print_string state lang;
  print_expr   state code;
  print_token  state rbracket "]"

and print_block_expr state {value; _} =
  let {block;kwd_with;expr} = value in
  print_block state block;
  print_token state kwd_with "with";
  print_expr  state expr;

and print_parameters state {value; _} =
  let {lpar; inside; rpar} = value in
  print_token   state lpar "(";
  print_nsepseq state ";" print_param_decl inside;
  print_token   state rpar ")"

and print_param_decl state = function
  ParamConst param_const -> print_param_const state param_const
| ParamVar   param_var   -> print_param_var   state param_var

and print_param_const state {value; _} =
  let {kwd_const; var; param_type} = value in
  print_token  state kwd_const "const";
  print_pvar   state var;
  print_option state print_type_annot param_type

and print_param_var state {value; _} =
  let {kwd_var; var; param_type} = value in
  print_token    state kwd_var "var";
  print_pvar     state var;
  print_option   state print_type_annot param_type

and print_block state block =
  let {enclosing; statements; terminator} = block.value in
  match enclosing with
    Block (kwd_block, lbrace, rbrace) ->
      print_token      state kwd_block "block";
      print_token      state lbrace "{";
      print_statements state statements;
      print_terminator state terminator;
      print_token      state rbrace "}"
  | BeginEnd (kwd_begin, kwd_end) ->
      print_token      state kwd_begin "begin";
      print_statements state statements;
      print_terminator state terminator;
      print_token      state kwd_end "end"

and print_data_decl state = function
  LocalConst       decl -> print_const_decl   state decl
| LocalVar         decl -> print_var_decl     state decl
| LocalFun         decl -> print_fun_decl     state decl
| LocalType        decl -> print_type_decl    state decl
| LocalModule      decl -> print_module_decl  state decl
| LocalModuleAlias decl -> print_module_alias state decl

and print_var_decl state {value; _} =
  let {attributes; kwd_var; pattern; var_type;
       assign; init; terminator} = value in
  print_attributes state attributes;
  print_token      state kwd_var "var";
  print_pattern    state pattern;
  print_option     state print_type_annot var_type;
  print_token      state assign ":=";
  print_expr       state init;
  print_terminator state terminator

and print_statements state sequence =
  print_nsepseq state ";" print_statement sequence

and print_statement state = function
  Instr instr -> print_instruction state instr
| Data  data  -> print_data_decl   state data

and print_instruction state = function
  Cond        {value; _} -> print_conditional state value
| CaseInstr   {value; _} -> print_case_instr state value
| Assign      assign     -> print_assignment state assign
| Loop        loop       -> print_loop state loop
| ProcCall    fun_call   -> print_fun_call state fun_call
| Skip        kwd_skip   -> print_token state kwd_skip "skip"
| RecordPatch {value; _} -> print_record_patch state value
| MapPatch    {value; _} -> print_map_patch state value
| SetPatch    {value; _} -> print_set_patch state value
| MapRemove   {value; _} -> print_map_remove state value
| SetRemove   {value; _} -> print_set_remove state value

and print_cond_expr state (node: cond_expr) =
  print_token      state node.kwd_if "if";
  print_expr       state node.test;
  print_token      state node.kwd_then "then";
  print_expr       state node.ifso;
  print_terminator state node.terminator;
  print_token      state node.kwd_else "else";
  print_expr       state node.ifnot

and print_conditional state (node: conditional) =
  print_token      state node.kwd_if "if";
  print_expr       state node.test;
  print_token      state node.kwd_then "then";
  print_if_clause  state node.ifso;
  print_terminator state node.terminator;
  print_token      state node.kwd_else "else";
  print_if_clause  state node.ifnot

and print_if_clause state = function
  ClauseInstr instr -> print_instruction state instr
| ClauseBlock block -> print_clause_block state block

and print_clause_block state = function
  LongBlock block ->
    print_block state block
| ShortBlock {value; _} ->
    let {lbrace; inside; rbrace} = value in
    let statements, terminator = inside in
    print_token      state lbrace "{";
    print_statements state statements;
    print_terminator state terminator;
    print_token      state rbrace "}"

and print_case_instr state (node : if_clause case) =
  let {kwd_case; expr; kwd_of; enclosing; lead_vbar; cases} = node in
  print_token       state kwd_case "case";
  print_expr        state expr;
  print_token       state kwd_of "of";
  match enclosing with
    Brackets (lbracket, rbracket) ->
      print_token       state lbracket "[";
      print_token_opt   state lead_vbar "|";
      print_cases_instr state cases;
      print_token       state rbracket "]"
  | End kwd_end ->
      print_token_opt   state lead_vbar "|";
      print_cases_instr state cases;
      print_token       state kwd_end "end"

and print_token_opt state = function
         None -> fun _ -> ()
| Some region -> print_token state region

and print_cases_instr state {value; _} =
  print_nsepseq state "|" print_case_clause_instr value

and print_case_clause_instr state {value; _} =
  let {pattern; arrow; rhs} = value in
  print_pattern   state pattern;
  print_token     state arrow "->";
  print_if_clause state rhs

and print_assignment state {value; _} =
  let {lhs; assign; rhs} = value in
  print_lhs   state lhs;
  print_token state assign ":=";
  print_rhs   state rhs

and print_rhs state e = print_expr state e

and print_lhs state = function
  Path path          -> print_path state path
| MapPath {value; _} -> print_map_lookup state value

and print_loop state = function
  While {value; _} -> print_while_loop state value
| For     for_loop -> print_for_loop   state for_loop

and print_while_loop state value =
  let {kwd_while; cond; block} = value in
  print_token state kwd_while "while";
  print_expr  state cond;
  print_block state block

and print_for_loop state = function
  ForInt     for_int     -> print_for_int     state for_int
| ForCollect for_collect -> print_for_collect state for_collect

and print_for_int state ({value; _} : for_int reg) =
  let {kwd_for; binder; assign; init; kwd_to; bound; step; block} = value in
  print_token      state kwd_for "for";
  print_var        state binder;
  print_token      state assign ":=";
  print_expr       state init;
  print_token      state kwd_to "to";
  print_expr       state bound;
  (match step with
     None -> ();
   | Some (kwd_step, expr) ->
       print_token  state kwd_step "step";
       print_expr   state expr);
  print_block      state block

and print_for_collect state ({value; _} : for_collect reg) =
  let {kwd_for; var; bind_to;
       kwd_in; collection; expr; block} = value in
  print_token      state kwd_for "for";
  print_var        state var;
  print_bind_to    state bind_to;
  print_token      state kwd_in "in";
  print_collection state collection;
  print_expr       state expr;
  print_block      state block

and print_collection state = function
  Map kwd_map ->
    print_token state kwd_map "map"
| Set kwd_set ->
    print_token state kwd_set "set"
| List kwd_list ->
    print_token state kwd_list "list"

and print_bind_to state = function
  Some (arrow, variable) ->
    print_token state arrow "->";
    print_var   state variable
| None -> ()

and print_expr state = function
  ECase    {value;_} -> print_case_expr state value
| ECond    {value;_} -> print_cond_expr state value
| EAnnot   {value;_} -> print_annot_expr state value
| ELogic   e -> print_logic_expr state e
| EArith   e -> print_arith_expr state e
| EString  e -> print_string_expr state e
| EList    e -> print_list_expr state e
| ESet     e -> print_set_expr state e
| EConstr  e -> print_constr_expr state e
| ERecord  e -> print_record_expr state e
| EUpdate  e -> print_update_expr state e
| EProj    e -> print_projection state e
| EModA   ma -> print_module_access print_expr state ma
| EMap     e -> print_map_expr state e
| EVar     v -> print_var state v
| ECall    e -> print_fun_call state e
| EBytes   b -> print_bytes state b
| ETuple   e -> print_tuple_expr state e
| EPar     e -> print_par_expr state e
| EFun     e -> print_fun_expr state e
| ECodeInj e -> print_code_inj state e
| EBlock   e -> print_block_expr state e

and print_annot_expr state node =
  let {inside; _} : annot_expr par = node in
  let expr, _, type_expr = inside in
  print_expr state expr;
  print_type_expr state type_expr

and print_case_expr state (node : expr case) =
  let {kwd_case; expr; kwd_of; enclosing; lead_vbar; cases} = node in
  print_token      state kwd_case "case";
  print_expr       state expr;
  print_token       state kwd_of "of";
  match enclosing with
    Brackets (lbracket, rbracket) ->
      print_token      state lbracket "[";
      print_token_opt  state lead_vbar "|";
      print_cases_expr state cases;
      print_token      state rbracket "]"
  | End kwd_end ->
      print_token_opt  state lead_vbar "|";
      print_cases_expr state cases;
      print_token      state kwd_end "end"

and print_cases_expr state {value; _} =
  print_nsepseq state "|" print_case_clause_expr value

and print_case_clause_expr state {value; _} =
  let {pattern; arrow; rhs} = value in
  print_pattern state pattern;
  print_token   state arrow "->";
  print_expr    state rhs

and print_map_expr state = function
  MapLookUp {value; _} -> print_map_lookup state value
| MapInj inj           -> print_injection  state print_binding inj
| BigMapInj inj        -> print_injection  state print_binding inj

and print_set_expr state = function
  SetInj inj -> print_injection state print_expr inj
| SetMem mem -> print_set_membership state mem

and print_set_membership state {value; _} =
  let {set; kwd_contains; element} = value in
  print_expr  state set;
  print_token state kwd_contains "contains";
  print_expr  state element

and print_map_lookup state {path; index} =
  let {lbracket; inside; rbracket} = index.value in
  print_path  state path;
  print_token state lbracket "[";
  print_expr  state inside;
  print_token state rbracket "]"

and print_path state = function
  Name var  -> print_var        state var
| Path path -> print_projection state path

and print_logic_expr state = function
  BoolExpr e -> print_bool_expr state e
| CompExpr e -> print_comp_expr state e

and print_bool_expr state = function
  Or {value = {arg1; op; arg2}; _} ->
    print_expr  state arg1;
    print_token state op "||";
    print_expr  state arg2
| And {value = {arg1; op; arg2}; _} ->
    print_expr  state arg1;
    print_token state op "&&";
    print_expr  state arg2
| Not {value = {op; arg}; _} ->
    print_token state op "not";
    print_expr  state arg

and print_comp_expr state = function
  Lt {value = {arg1; op; arg2}; _} ->
    print_expr  state arg1;
    print_token state op "<";
    print_expr  state arg2
| Leq {value = {arg1; op; arg2}; _} ->
    print_expr  state arg1;
    print_token state op "<=";
    print_expr  state arg2
| Gt {value = {arg1; op; arg2}; _} ->
    print_expr  state arg1;
    print_token state op ">";
    print_expr  state arg2
| Geq {value = {arg1; op; arg2}; _} ->
    print_expr  state arg1;
    print_token state op ">=";
    print_expr  state arg2
| Equal {value = {arg1; op; arg2}; _} ->
    print_expr  state arg1;
    print_token state op "=";
    print_expr  state arg2
| Neq {value = {arg1; op; arg2}; _} ->
    print_expr  state arg1;
    print_token state op "=/=";
    print_expr  state arg2

and print_arith_expr state = function
  Add {value = {arg1; op; arg2}; _} ->
    print_expr  state arg1;
    print_token state op "+";
    print_expr  state arg2
| Sub {value = {arg1; op; arg2}; _} ->
    print_expr  state arg1;
    print_token state op "-";
    print_expr  state arg2
| Mult {value = {arg1; op; arg2}; _} ->
    print_expr  state arg1;
    print_token state op "*";
    print_expr  state arg2
| Div {value = {arg1; op; arg2}; _} ->
    print_expr  state arg1;
    print_token state op "/";
    print_expr  state arg2
| Mod {value = {arg1; op; arg2}; _} ->
    print_expr  state arg1;
    print_token state op "mod";
    print_expr  state arg2
| Neg {value = {op; arg}; _} ->
    print_token state op "-";
    print_expr  state arg
| Int i
| Nat i
| Mutez i -> print_int state i

and print_string_expr state = function
  Cat {value = {arg1; op; arg2}; _} ->
    print_expr  state arg1;
    print_token state op "^";
    print_expr  state arg2
| String s ->
    print_string state s
| Verbatim v ->
    print_verbatim state v

and print_list_expr state = function
  ECons {value = {arg1; op; arg2}; _} ->
    print_expr  state arg1;
    print_token state op "#";
    print_expr  state arg2
| EListComp e -> print_injection state print_expr e
| ENil e -> print_nil state e

and print_constr_expr state {value; _} =
  let constr, arguments = value in
  print_constr state constr;
  match arguments with
    None -> ()
  | Some arg -> print_tuple_expr state arg

and print_record_expr state =
  print_ne_injection state print_field_assignment

and print_field_assignment state {value; _} =
  let {field_name; assignment; field_expr} = value in
  print_var   state field_name;
  print_token state assignment "=";
  print_expr  state field_expr

and print_field_path_assignment state {value; _} =
  let {field_path; assignment; field_expr} = value in
  print_path  state field_path;
  print_token state assignment "=";
  print_expr  state field_expr

and print_update_expr state {value; _} =
  let {record; kwd_with; updates} = value in
  print_path state record;
  print_token state kwd_with "with";
  print_ne_injection state print_field_path_assignment updates

and print_projection state {value; _} =
  let {struct_name; selector; field_path} = value in
  print_var        state struct_name;
  print_token      state selector ".";
  print_field_path state field_path

and print_module_access : type a.(state -> a -> unit ) -> state -> a module_access reg -> unit =
fun f state {value; _} ->
  let {module_name; selector; field} = value in
  print_var     state module_name;
  print_token   state selector ".";
  f             state field;

and print_field_path state sequence =
  print_nsepseq state "." print_selection sequence

and print_selection state = function
  FieldName name -> print_var state name
| Component int  -> print_int state int

and print_record_patch state node =
  let {kwd_patch; path; kwd_with; record_inj} = node in
  print_token        state kwd_patch "patch";
  print_path         state path;
  print_token        state kwd_with "with";
  print_ne_injection state print_field_assignment record_inj

and print_set_patch state node =
  let {kwd_patch; path; kwd_with; set_inj} = node in
  print_token        state kwd_patch "patch";
  print_path         state path;
  print_token        state kwd_with "with";
  print_ne_injection state print_expr set_inj

and print_map_patch state node =
  let {kwd_patch; path; kwd_with; map_inj} = node in
  print_token        state kwd_patch "patch";
  print_path         state path;
  print_token        state kwd_with "with";
  print_ne_injection state print_binding map_inj

and print_map_remove state node =
  let {kwd_remove; key; kwd_from; kwd_map; map} = node in
  print_token state kwd_remove "remove";
  print_expr  state key;
  print_token state kwd_from "from";
  print_token state kwd_map "map";
  print_path  state map

and print_set_remove state node =
  let {kwd_remove; element; kwd_from; kwd_set; set} = node in
  print_token state kwd_remove "remove";
  print_expr  state element;
  print_token state kwd_from "from";
  print_token state kwd_set "set";
  print_path  state set

and print_injection :
  'a.state -> (state -> 'a -> unit) -> 'a injection reg -> unit =
  fun state print {value; _} ->
    let {kind; enclosing; elements; terminator} = value in
    print_injection_kwd state kind;
    match enclosing with
      Brackets (lbracket, rbracket) ->
        print_token      state lbracket "[";
        print_sepseq     state ";" print elements;
        print_terminator state terminator;
        print_token      state rbracket "]"
    | End kwd_end ->
        print_sepseq     state ";" print elements;
        print_terminator state terminator;
        print_token      state kwd_end "end"

and print_injection_kwd state = function
  InjSet    kwd_set     -> print_token state kwd_set "set"
| InjMap    kwd_map     -> print_token state kwd_map "map"
| InjBigMap kwd_big_map -> print_token state kwd_big_map "big_map"
| InjList   kwd_list    -> print_token state kwd_list "list"
| InjRecord   kwd_list    -> print_token state kwd_list "record"

and print_ne_injection :
  'a.state -> (state -> 'a -> unit) -> 'a ne_injection reg -> unit =
  fun state print {value; _} ->
    let {kind; enclosing; ne_elements; terminator; attributes} = value in
    print_attributes       state attributes;
    print_ne_injection_kwd state kind;
    match enclosing with
      Brackets (lbracket, rbracket) ->
        print_token      state lbracket "[";
        print_nsepseq    state ";" print ne_elements;
        print_terminator state terminator;
        print_token      state rbracket "]"
    | End kwd_end ->
        print_nsepseq    state ";" print ne_elements;
        print_terminator state terminator;
        print_token      state kwd_end "end"

and print_ne_injection_kwd state = function
  NEInjSet    kwd_set        -> print_token state kwd_set "set"
| NEInjMap    kwd_map        -> print_token state kwd_map "map"
| NEInjRecord kwd_record     -> print_token state kwd_record "record"

and print_binding state {value; _} =
  let {source; arrow; image} = value in
  print_expr  state source;
  print_token state arrow "->";
  print_expr  state image

and print_tuple_expr state {value; _} =
  let {lpar; inside; rpar} = value in
  print_token   state lpar "(";
  print_nsepseq state "," print_expr inside;
  print_token   state rpar ")"

and print_nil state value = print_token state value "nil"

and print_fun_call state {value; _} =
  let expr, arguments = value in
  print_expr       state expr;
  print_tuple_expr state arguments

and print_par_expr state {value; _} =
  let {lpar; inside; rpar} = value in
  print_token state lpar "(";
  print_expr  state inside;
  print_token state rpar ")"

and print_pattern state = function
  PVar var         -> print_pvar state var
| PInt i           -> print_int state i
| PNat n           -> print_nat state n
| PBytes b         -> print_bytes state b
| PString s        -> print_string state s
| PList pattern    -> print_list_pattern state pattern
| PTuple ptuple    -> print_ptuple state ptuple
| PConstr pattern  -> print_constr_pattern state pattern
| PRecord record   -> print_record_pattern state record

and print_record_pattern state record_pattern =
  print_injection state print_field_pattern record_pattern

and print_field_pattern state {value; _} =
  let {field_name; eq; pattern} = value in
  print_var     state field_name;
  print_token   state eq "=";
  print_pattern state pattern

and print_constr_pattern state {value; _} =
  let constr, arg = value in
  print_constr state constr;
  match arg with
    None -> ()
  | Some tuple -> print_ptuple state tuple

and print_list_pattern state = function
  PListComp comp ->
   print_injection state print_pattern comp
| PNil kwd_nil ->
    print_token state kwd_nil "nil"
| PParCons cons ->
    print_par_cons state cons
| PCons {value; _} ->
   print_nsepseq state "#" print_pattern value

and print_par_cons state {value; _} =
  let {lpar; inside; rpar} = value in
  let head, cons, tail = inside in
  print_token   state lpar "(";
  print_pattern state head;
  print_token   state cons "#";
  print_pattern state tail;
  print_token   state rpar ")"

and print_ptuple state {value; _} =
  let {lpar; inside; rpar} = value in
  print_token   state lpar "(";
  print_nsepseq state "," print_pattern inside;
  print_token   state rpar ")"

and print_terminator state = function
  Some semi -> print_token state semi ";"
| None -> ()

(* Conversion to string *)

let to_string ~offsets ~mode printer node =
  let buffer = Buffer.create 131 in
  let state = mk_state ~offsets ~mode ~buffer in
  let () = printer state node
  in Buffer.contents buffer

let tokens_to_string ~offsets ~mode =
  to_string ~offsets ~mode print_tokens

let path_to_string ~offsets ~mode =
  to_string ~offsets ~mode print_path

let pattern_to_string ~offsets ~mode =
  to_string ~offsets ~mode print_pattern

let instruction_to_string ~offsets ~mode =
  to_string ~offsets ~mode print_instruction

let type_expr_to_string ~offsets ~mode =
  to_string ~offsets ~mode print_type_expr

(* Pretty-printing the CST *)

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

let pp_verbatim state {value=name; region} =
  let reg  = compact state region in
  let node = sprintf "%s{|%s|} (%s)\n" state#pad_path name reg
  in Buffer.add_string state#buffer node

let pp_loc_node state name region =
  pp_ident state {value=name; region}

let rec pp_cst state {decl; _} =
  let apply len rank =
    pp_declaration (state#pad len rank) in
  let decls = Utils.nseq_to_list decl in
  pp_node state "<ast>";
  List.iteri (List.length decls |> apply) decls

and pp_declaration state = function
  TypeDecl {value; region} ->
    pp_loc_node  state "TypeDecl" region;
    pp_type_decl state value
| ConstDecl {value; region} ->
    pp_loc_node state "ConstDecl" region;
    pp_const_decl state value
| FunDecl {value; region} ->
    pp_loc_node state "FunDecl" region;
    pp_fun_decl state value
| ModuleDecl {value; region} ->
    pp_loc_node state "ModuleDecl" region;
    pp_mod_decl state value
| ModuleAlias {value; region} ->
    pp_loc_node  state "ModuleAlias" region;
    pp_mod_alias state value
| Directive dir ->
    let region, string = Directive.project dir in
    pp_loc_node state "Directive" region;
    pp_node state string

and pp_type_decl state (decl : type_decl) =
  let () =
    let state = state#pad 2 0 in
    pp_node  state "<name>";
    pp_ident (state#pad 1 0) decl.name in
  let () =
    let state = state#pad 2 1 in
    pp_node  state "<type_expr>";
    pp_type_expr (state#pad 1 0) decl.type_expr
  in ()

and pp_mod_decl state (decl : module_decl) =
  let () =
    let state = state#pad 2 0 in
    pp_node  state "<name>";
    pp_ident (state#pad 1 0) decl.name in
  let () =
    let state = state#pad 2 1 in
    pp_node  state "<module>";
    pp_cst (state#pad 1 0) decl.module_
  in ()

and pp_mod_alias state (decl : module_alias) =
  let () =
    let state = state#pad 2 0 in
    pp_node  state "<alias>";
    pp_ident (state#pad 1 0) decl.alias in
  let () =
    let state = state#pad 2 1 in
    let binders     = Utils.nsepseq_to_list decl.binders in
    let len            = List.length binders in
    let apply len rank = pp_ident (state#pad len rank) in
    pp_node  state "<module>";
    List.iteri (apply len) binders
  in ()

and pp_fun_decl state decl =
  let arity = if decl.kwd_recursive = None then 0 else 1 in
  let arity = if decl.ret_type = None then arity else arity+1 in
  let arity = if decl.attributes = [] then arity else arity+1 in
  let arity = arity + 3
  and rank = 0 in
  let rank =
    match decl.kwd_recursive with
        None -> rank
    | Some _ -> let state = state#pad arity rank in
                pp_node state "recursive";
                rank+1 in
  let rank =
    let state = state#pad arity rank in
    pp_ident state decl.fun_name;
    rank + 1 in
  let rank =
    let state = state#pad arity rank in
    pp_node state "<parameters>";
    pp_parameters state decl.param;
    rank + 1 in
  let rank =
    match decl.ret_type with
      None -> rank
    | Some (_, t_expr) ->
       let state = state#pad arity rank in
       pp_node state "<return type>";
       pp_type_expr (state#pad 1 0) t_expr;
       rank+1 in
  let rank =
    let state = state#pad arity rank in
    pp_node state "<return>";
    pp_expr (state#pad 1 0) decl.return;
    rank+1 in
  let () =
    let attr = decl.attributes in
    if attr <> [] then
      let state = state#pad arity rank in
      pp_node state "<attributes>";
      let length         = List.length attr in
      let apply len rank = pp_ident (state#pad len rank)
      in List.iteri (apply length) attr
  in ()

and pp_const_decl state decl =
  let arity = if decl.const_type = None then 0 else 1 in
  let arity = if decl.attributes = [] then arity else arity+1 in
  let arity = arity + 2 in
  let rank = 0 in
  let rank =
    pp_pattern (state#pad arity 0) decl.pattern; rank+1 in
  let rank =
    pp_type_annot (state#pad arity rank) rank decl.const_type in
  let rank =
    pp_expr (state#pad arity rank) decl.init; rank+1 in
  let rank =
    let attr = decl.attributes in
    if attr <> [] then
      let state = state#pad arity rank in
      pp_node state "<attributes>";
      let length         = List.length attr in
      let apply len rank = pp_ident (state#pad len rank)
      in List.iteri (apply length) attr; rank+1
    else rank
  in ignore rank

and pp_type_expr state = function
  TProd cartesian ->
    pp_loc_node state "TProd" cartesian.region;
    pp_cartesian state cartesian
| TVar v ->
    pp_node state "TVar";
    pp_ident (state#pad 1 0) v
| TPar {value; region} ->
    pp_loc_node state "TPar" region;
    pp_type_expr (state#pad 1 0) value.inside
| TApp {value=name,tuple; region} ->
    pp_loc_node state "TApp" region;
    pp_ident (state#pad 1 0) name;
    pp_type_tuple (state#pad 2 1) tuple
| TFun {value; region} ->
    pp_loc_node state "TFun" region;
    let apply len rank =
      pp_type_expr (state#pad len rank) in
    let domain, _, range = value in
    List.iteri (apply 2) [domain; range]
| TSum {value; region} ->
    pp_loc_node state "TSum" region;
    pp_sum_type state value
| TRecord {value; region} ->
    pp_loc_node state "TRecord" region;
    pp_ne_injection pp_field_decl state value
| TString s ->
    pp_node   state "TString";
    pp_string (state#pad 1 0) s
| TInt s ->
    pp_node   state "TInt";
    pp_int (state#pad 1 0) s
| TModA {value; region} ->
    pp_loc_node state "TModA" region;
    pp_module_access pp_type_expr state value

and pp_pvar state {value; _} =
  let {variable; attributes} = value in
  if attributes = [] then
    pp_ident state variable
  else
    (pp_node       state "PVar";
     pp_ident      (state#pad 2 0) variable;
     pp_attributes (state#pad 2 1) attributes)

and pp_sum_type state {variants; attributes; _} =
  let variants = Utils.nsepseq_to_list variants in
  let arity    = List.length variants in
  let arity    = if attributes = [] then arity else arity+1 in
  let apply arity rank variant =
    let state = state#pad arity rank in
    pp_variant state variant.value in
  let () = List.iteri (apply arity) variants in
  if attributes <> [] then
    let state = state#pad arity (arity-1)
    in pp_attributes state attributes

and pp_cartesian state {value; _} =
  let apply len rank =
    pp_type_expr (state#pad len rank) in
  let components = Utils.nsepseq_to_list value
  in List.iteri (List.length components |> apply) components

and pp_attributes state attributes =
  pp_node state "<attributes>";
  let length         = List.length attributes in
  let apply len rank = pp_ident (state#pad len rank)
  in List.iteri (apply length) attributes

and pp_variant state {constr; arg; attributes=attr} =
  let arity = if attr = [] then 0 else 1 in
  let arity = if arg = None then arity else arity + 1 in
  let rank  = 0 in
  let () = pp_ident state constr in
  let rank =
    match arg with
      None -> rank
    | Some (_,c) -> pp_type_expr (state#pad arity rank) c; rank+1 in
  let rank = if attr <> [] then
               pp_attributes (state#pad arity rank) attr; rank+1
  in ignore rank

and pp_field_decl state {value; _} =
  let arity = if value.attributes = [] then 1 else 2 in
  pp_ident     state value.field_name;
  pp_type_expr (state#pad arity 0) value.field_type;
  if value.attributes <> [] then
    pp_attributes (state#pad arity 1) value.attributes

and pp_type_tuple state {value; _} =
  let components = Utils.nsepseq_to_list value.inside in
  let apply len rank = pp_type_expr (state#pad len rank)
  in List.iteri (List.length components |> apply) components

and pp_fun_expr state (expr: fun_expr) =
  let arity = if expr.ret_type = None then 2 else 3 in
  let rank = 0 in
  let rank =
    let state = state#pad arity rank in
    pp_node state "<parameters>";
    pp_parameters state expr.param;
    rank + 1 in
  let rank =
    match expr.ret_type with
      None -> rank
    | Some (_, t_expr) ->
        let state = state#pad arity rank in
        pp_node state "<return type>";
        pp_type_expr (state#pad 1 0) t_expr;
        rank + 1 in
  let () =
    let state = state#pad arity rank in
    pp_node state "<return>";
    pp_expr (state#pad 1 0) expr.return
  in ()

and pp_code_inj state node =
  let () =
    let state = state#pad 2 0 in
    pp_node state "<language>";
    pp_string (state#pad 1 0) node.language.value in
  let () =
    let state = state#pad 2 1 in
    pp_node state "<code>";
    pp_expr (state#pad 1 0) node.code
  in ()

and pp_block_expr state node =
  let {block; expr; _} : block_with = node in
  let () =
    let state = state#pad 2 0 in
    pp_node state "<block>";
    pp_statements state block.value.statements in
  let () =
    let state = state#pad 2 1 in
    pp_node state "<expr>";
    pp_expr (state#pad 1 0) expr
  in ()

and pp_parameters state {value; _} =
  let params = Utils.nsepseq_to_list value.inside in
  let arity  = List.length params in
  let apply len rank = pp_param_decl (state#pad len rank)
  in List.iteri (apply arity) params

and pp_param_decl state = function
  ParamConst {value; region} ->
    let arity = if value.param_type = None then 1 else 2 in
    pp_loc_node state "ParamConst" region;
    pp_pvar (state#pad arity 0) value.var;
    ignore (pp_type_annot (state#pad arity 1) 1 value.param_type)
| ParamVar {value; region} ->
    let arity = if value.param_type = None then 1 else 2 in
    pp_loc_node state "ParamVar" region;
    pp_pvar (state#pad 2 0) value.var;
    ignore (pp_type_annot (state#pad arity 1) 1 value.param_type)

and pp_statements state statements =
  let statements = Utils.nsepseq_to_list statements in
  let length     = List.length statements in
  let apply len rank = pp_statement (state#pad len rank)
  in List.iteri (apply length) statements

and pp_statement state = function
  Instr instr ->
    pp_node state "Instr";
    pp_instruction (state#pad 1 0) instr
| Data data_decl ->
    pp_node state "Data";
    pp_data_decl (state#pad 1 0) data_decl
and pp_instruction state = function
  Cond {value; region} ->
    pp_loc_node state "Cond" region;
    pp_conditional state value
| CaseInstr {value; region} ->
    pp_loc_node state "CaseInstr" region;
    pp_case pp_if_clause state value
| Assign {value; region} ->
    pp_loc_node state "Assign" region;
    pp_assignment state value
| Loop loop ->
    pp_node state "Loop";
    pp_loop (state#pad 1 0) loop
| ProcCall {value; region} ->
    pp_loc_node state "ProcCall" region;
    pp_fun_call state value
| Skip kwd_skip ->
    pp_loc_node state "Skip" kwd_skip#region
| RecordPatch {value; region} ->
    pp_loc_node state "RecordPatch" region;
    pp_record_patch state value
| MapPatch {value; region} ->
    pp_loc_node state "MapPatch" region;
    pp_map_patch state value
| SetPatch {value; region} ->
    pp_loc_node state "SetPatch" region;
    pp_set_patch state value
| MapRemove {value; region} ->
    pp_loc_node state "MapRemove" region;
    pp_map_remove state value
| SetRemove {value; region} ->
    pp_loc_node state "SetRemove" region;
    pp_set_remove state value

and pp_cond_expr state (cond: cond_expr) =
  let () =
    let state = state#pad 3 0 in
    pp_node state "<condition>";
    pp_expr (state#pad 1 0) cond.test in
  let () =
    let state = state#pad 3 1 in
    pp_node state "<true>";
    pp_expr (state#pad 1 0) cond.ifso in
  let () =
    let state = state#pad 3 2 in
    pp_node state "<false>";
    pp_expr (state#pad 1 0) cond.ifnot
  in ()

and pp_conditional state (cond: conditional) =
  let () =
    let state = state#pad 3 0 in
    pp_node state "<condition>";
    pp_expr (state#pad 1 0) cond.test in
  let () =
    let state = state#pad 3 1 in
    pp_node state "<true>";
    pp_if_clause (state#pad 1 0) cond.ifso in
  let () =
    let state = state#pad 3 2 in
    pp_node state "<false>";
    pp_if_clause (state#pad 1 0) cond.ifnot
  in ()

and pp_if_clause state = function
  ClauseInstr instr ->
    pp_node state "ClauseInstr";
    pp_instruction (state#pad 1 0) instr
| ClauseBlock block ->
    pp_node state "ClauseBlock";
    pp_clause_block (state#pad 1 0) block

and pp_clause_block state = function
  LongBlock {value; region} ->
    pp_loc_node state "LongBlock" region;
    pp_statements state value.statements
| ShortBlock {value; region} ->
    pp_loc_node state "ShortBlock" region;
    pp_statements state (fst value.inside)

and pp_case :
  'a.(state -> 'a -> unit) -> state -> 'a case -> unit =
  fun printer state case ->
    let clauses = Utils.nsepseq_to_list case.cases.value in
    let clauses = List.map (fun {value; _} -> value) clauses in
    let length  = List.length clauses + 1 in
    let apply len rank =
      let state = state#pad len (rank+1)
      in pp_case_clause printer state
    in pp_expr (state#pad length 0) case.expr;
    List.iteri (apply length) clauses

and pp_case_clause :
  'a.(state -> 'a -> unit) -> state -> 'a case_clause -> unit =
  fun printer state clause ->
    pp_node state "<clause>";
    pp_pattern (state#pad 2 0) clause.pattern;
    printer (state#pad 2 1) clause.rhs

and pp_pattern state = function
  PConstr pattern ->
    pp_node state "PConstr";
    pp_constr_pattern (state#pad 1 0) pattern
| PVar p -> pp_pvar state p
| PInt n ->
    pp_node state "PInt";
    pp_int state n
| PNat n ->
    pp_node state "PNat";
    pp_int state n
| PBytes b ->
    pp_node state "PBytes";
    pp_bytes state b
| PString s ->
    pp_node state "PString";
    pp_ident (state#pad 1 0) s
| PList plist ->
    pp_node state "PList";
    pp_list_pattern (state#pad 1 0) plist
| PTuple {value; region} ->
    pp_loc_node state "PTuple" region;
    pp_tuple_pattern (state#pad 1 0) value
| PRecord {value; _} ->
    pp_node state "PRecord";
    pp_injection pp_field_pattern state value

and pp_field_pattern state {value; _} =
  pp_node    state value.field_name.value;
  pp_pattern (state#pad 1 0) value.pattern

and pp_bytes state {value=lexeme,hex; region} =
  pp_loc_node (state#pad 2 0) lexeme region;
  pp_node     (state#pad 2 1) (Hex.show hex)

and pp_int state {value=lexeme,z; region} =
  pp_loc_node (state#pad 2 0) lexeme region;
  pp_node     (state#pad 2 1) (Z.to_string z)

and pp_constr_pattern state {value; _} =
  let constr, pat_opt = value in
  pp_ident state constr;
  match pat_opt with
               None -> ()
  | Some {value; _} -> pp_tuple_pattern state value

and pp_list_pattern state = function
  PListComp {value; region} ->
    pp_loc_node state "PListComp" region;
    pp_injection pp_pattern (state#pad 1 0) value
| PNil kwd_nil ->
    pp_loc_node state "PNil" kwd_nil#region
| PParCons {value; region} ->
    pp_loc_node state "PParCons" region;
    pp_bin_cons (state#pad 1 0) value.inside
| PCons {value; region} ->
    let patterns = Utils.nsepseq_to_list value in
    let length   = List.length patterns in
    let apply len rank =
      pp_pattern (state#pad len rank) in
    pp_loc_node state "PCons" region;
    List.iteri (apply length) patterns

and pp_bin_cons state (head, _, tail) =
  pp_pattern (state#pad 2 0) head;
  pp_pattern (state#pad 2 1) tail

and pp_injection :
  'a.(state -> 'a -> unit) -> state -> 'a injection -> unit =
  fun printer state inj ->
    let elements       = Utils.sepseq_to_list inj.elements in
    let length         = List.length elements in
    let apply len rank = printer (state#pad len rank)
    in List.iteri (apply length) elements

and pp_ne_injection :
  'a.(state -> 'a -> unit) -> state -> 'a ne_injection -> unit =
  fun printer state inj ->
    let ne_elements    = Utils.nsepseq_to_list inj.ne_elements in
    let length         = List.length ne_elements in
    let arity          = if inj.attributes = [] then length else length + 1
    and apply len rank = printer (state#pad len rank)
    in List.iteri (apply arity) ne_elements;
       if inj.attributes <> [] then
         let state = state#pad arity (arity-1)
         in pp_attributes state inj.attributes

and pp_tuple_pattern state tuple =
  let patterns       = Utils.nsepseq_to_list tuple.inside in
  let length         = List.length patterns in
  let apply len rank = pp_pattern (state#pad len rank)
  in List.iteri (apply length) patterns

and pp_assignment state asgn =
  pp_lhs  (state#pad 2 0) asgn.lhs;
  pp_expr (state#pad 2 1) asgn.rhs

and pp_lhs state = function
  Path path ->
    pp_node state "Path";
    pp_path (state#pad 1 0) path
| MapPath {value; region} ->
    pp_loc_node state "MapPath" region;
    pp_map_lookup state value

and pp_path state = function
  Name name ->
    pp_node state "Name";
    pp_ident (state#pad 1 0) name
| Path {value; region} ->
    pp_loc_node state "Path" region;
    pp_projection state value

and pp_projection state proj =
  let selections     = Utils.nsepseq_to_list proj.field_path in
  let len            = List.length selections in
  let apply len rank = pp_selection (state#pad len rank) in
  pp_ident (state#pad (1+len) 0) proj.struct_name;
  List.iteri (apply len) selections

and pp_module_access
    : type a. (state -> a -> unit ) -> state -> a module_access -> unit
  = fun printer state access ->
      pp_ident (state#pad 2 0) access.module_name;
      printer  (state#pad 2 1) access.field

and pp_update state update =
  pp_path (state#pad 2 0) update.record;
  pp_ne_injection pp_field_path_assignment state update.updates.value

and pp_selection state = function
  FieldName name ->
    pp_node state "FieldName";
    pp_ident (state#pad 1 0) name
| Component comp ->
    pp_node state "Component";
    pp_int state comp

and pp_map_lookup state lookup =
  pp_path (state#pad 2 0) lookup.path;
  pp_expr (state#pad 2 1) lookup.index.value.inside

and pp_loop state = function
  While {value; _} ->
    pp_node state "<while>";
    let () =
      let state = state#pad 2 0 in
      pp_node state "<condition>";
      pp_expr (state#pad 1 0) value.cond in
    let () =
      let state = state#pad 2 1 in
      let statements = value.block.value.statements in
      pp_node state "<statements>";
      pp_statements state statements
    in ()
| For for_loop ->
    pp_node state "<for>";
    pp_for_loop (state#pad 1 0) for_loop

and pp_for_loop state = function
  ForInt {value; region} ->
    pp_loc_node state "ForInt" region;
    pp_for_int state value
| ForCollect {value; region} ->
    pp_loc_node state "ForCollect" region;
    pp_for_collect state value

and pp_for_int state for_int =
  let {binder; init; bound; step; block; _} = for_int in
  let arity =
    match step with None -> 3 | Some _ -> 4 in
  let () =
    let state = state#pad arity 0 in
    pp_node state "<init>";
    pp_ident (state#pad 2 0) binder;
    pp_expr  (state#pad 2 1) init
    in
  let () =
    let state = state#pad arity 1 in
    pp_node state "<bound>";
    pp_expr (state#pad 1 0) bound in
  let () =
    match step with
      None -> ()
    | Some (_, expr) ->
        let state = state#pad arity 2 in
        pp_node state "<step>";
        pp_expr (state#pad 1 0) expr in
  let () =
    let state = state#pad arity (arity-1) in
    let statements = block.value.statements in
    pp_node state "<statements>";
    pp_statements state statements
  in ()

and pp_for_collect state collect =
  let () =
    let state = state#pad 3 0 in
    match collect.bind_to with
      None ->
        pp_ident state collect.var
    | Some (_, var) ->
        pp_var_binding state (collect.var, var) in
  let () =
    let state = state#pad 3 1 in
    pp_node state "<collection>";
    pp_collection (state#pad 2 0) collect.collection;
    pp_expr (state#pad 1 0) collect.expr in
  let () =
    let state = state#pad 3 2 in
    let statements = collect.block.value.statements in
    pp_node state "<statements>";
    pp_statements state statements
  in ()

and pp_collection state = function
  Map  kwd_map -> pp_loc_node state "map"   kwd_map#region
| Set  kwd_set -> pp_loc_node state "set"   kwd_set#region
| List kwd_list -> pp_loc_node state "list" kwd_list#region

and pp_var_binding state (source, image) =
  pp_node  state "<binding>";
  pp_ident (state#pad 2 0) source;
  pp_ident (state#pad 2 1) image

and pp_fun_call state (expr, args) =
  let args           = Utils.nsepseq_to_list args.value.inside in
  let arity          = List.length args in
  let apply len rank = pp_expr (state#pad len rank)
  in pp_expr (state#pad (1+arity) 0) expr;
     List.iteri (apply arity) args

and pp_record_patch state patch =
  pp_path (state#pad 2 0) patch.path;
  pp_ne_injection pp_field_assignment state patch.record_inj.value

and pp_field_assignment state {value; _} =
  pp_node  state "<field assignment>";
  pp_ident (state#pad 2 0) value.field_name;
  pp_expr  (state#pad 2 1) value.field_expr

and pp_field_path_assignment state {value; _} =
  let {field_path; field_expr; _} = value in
  pp_node state "<update>";
  pp_path (state#pad 2 0) field_path;
  pp_expr (state#pad 2 1) field_expr

and pp_map_patch state patch =
  pp_path (state#pad 2 0) patch.path;
  pp_ne_injection pp_binding state patch.map_inj.value

and pp_binding state {value; _} =
  let source, image = value.source, value.image in
  pp_node state "<binding>";
  pp_expr (state#pad 2 0) source;
  pp_expr (state#pad 2 1) image

and pp_set_patch state patch =
  pp_path (state#pad 2 0) patch.path;
  pp_ne_injection pp_expr state patch.set_inj.value

and pp_map_remove state rem =
  pp_expr (state#pad 2 0) rem.key;
  pp_path (state#pad 2 1) rem.map

and pp_set_remove state rem =
  pp_expr (state#pad 2 0) rem.element;
  pp_path (state#pad 2 1) rem.set

and pp_data_decl state = function
  LocalConst {value; region} ->
    pp_loc_node state "LocalConst" region;
    pp_const_decl state value
| LocalVar {value; region} ->
    pp_loc_node state "LocalVar" region;
    pp_var_decl state value
| LocalFun {value; region} ->
    pp_loc_node state "LocalFun" region;
    pp_fun_decl state value
| LocalType type_decl ->
    pp_node state "Type";
    pp_type_decl (state#pad 1 0) type_decl.value
| LocalModule module_decl ->
    pp_node state "Module";
    pp_mod_decl (state#pad 1 0) module_decl.value
| LocalModuleAlias module_alias ->
    pp_node state "Module";
    pp_mod_alias (state#pad 1 0) module_alias.value


and pp_var_decl state decl =
  let arity = if decl.var_type = None then 2 else 3 in
  let rank = 0 in
  let rank = pp_pattern (state#pad arity rank) decl.pattern; rank+1 in
  let rank = pp_type_annot (state#pad arity rank) rank decl.var_type
  in pp_expr (state#pad arity rank) decl.init

and pp_expr state = function
  ECase {value; region} ->
    pp_loc_node state "ECase" region;
    pp_case pp_expr state value
| ECond {value; region} ->
    pp_loc_node state "ECond" region;
    pp_cond_expr state value
| EAnnot {value; region} ->
    pp_loc_node state "EAnnot" region;
    pp_annotated state value.inside
| ELogic e_logic ->
    pp_node state "ELogic";
    pp_e_logic (state#pad 1 0) e_logic
| EArith e_arith ->
    pp_node state "EArith";
    pp_arith_expr (state#pad 1 0) e_arith
| EString e_string ->
    pp_node state "EString";
    pp_string_expr (state#pad 1 0) e_string
| EList e_list ->
    pp_node state "EList";
    pp_list_expr (state#pad 1 0) e_list
| ESet e_set ->
    pp_node state "ESet";
    pp_set_expr (state#pad 1 0) e_set
| EConstr e_constr ->
    pp_node state "EConstr";
    pp_constr_expr (state#pad 1 0) e_constr
| ERecord {value; region} ->
    pp_loc_node state "ERecord" region;
    pp_ne_injection pp_field_assignment state value
| EProj {value; region} ->
    pp_loc_node state "EProj" region;
    pp_projection state value
| EModA {value; region} ->
    pp_loc_node state "EModA" region;
    pp_module_access pp_expr state value
| EUpdate {value; region} ->
    pp_loc_node state "EUpdate" region;
    pp_update state value
| EMap e_map ->
    pp_node state "EMap";
    pp_map_expr (state#pad 1 0) e_map
| EVar v ->
    pp_node state "EVar";
    pp_ident (state#pad 1 0) v
| ECall {value; region} ->
    pp_loc_node state "ECall" region;
    pp_fun_call state value
| EBytes b ->
    pp_node state "EBytes";
    pp_bytes state b
| ETuple e_tuple ->
    pp_node state "ETuple";
    pp_tuple_expr state e_tuple
| EPar {value; region} ->
    pp_loc_node state "EPar" region;
    pp_expr (state#pad 1 0) value.inside
| EFun {value; region} ->
    pp_loc_node state "EFun" region;
    pp_fun_expr state value;
| ECodeInj {value; region} ->
    pp_loc_node state "ECodeInj" region;
    pp_code_inj state value;
| EBlock {value; region} ->
    pp_loc_node state "EBlock" region;
    pp_block_expr state value;

and pp_list_expr state = function
  ECons {value; region} ->
    pp_loc_node state "ECons" region;
    pp_expr (state#pad 2 0) value.arg1;
    pp_expr (state#pad 2 1) value.arg2
| ENil kwd_nil ->
    pp_loc_node state "ENil" kwd_nil#region
| EListComp {value; region} ->
    pp_loc_node state "EListComp" region;
    if value.elements = None then
      pp_node (state#pad 1 0) "[]"
    else pp_injection pp_expr state value

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
| Nat n ->
    pp_node state "Nat";
    pp_int  state n
| Mutez m ->
    pp_node state "Mutez";
    pp_int  state m

and pp_set_expr state = function
  SetInj {value; region} ->
    pp_loc_node  state "SetInj" region;
    pp_injection pp_expr state value
| SetMem {value; region} ->
    pp_loc_node state "SetMem" region;
    pp_expr     (state#pad 2 0) value.set;
    pp_expr     (state#pad 2 1) value.element

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
| Not {value; region} ->
    pp_loc_node state "Not" region;
    pp_expr     (state#pad 1 0) value.arg

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
    pp_bin_op "Equal" region state  value
| Neq {value; region} ->
    pp_bin_op "Neq" region state value

and pp_constr_expr state {value; _} =
  let constr, args_opt = value in
  pp_ident state constr;
  match args_opt with
         None -> ()
  | Some args -> pp_tuple_expr state args

and pp_map_expr state = function
  MapLookUp {value; region} ->
    pp_loc_node   state "MapLookUp" region;
    pp_map_lookup state value
| MapInj {value; region} | BigMapInj {value; region} ->
    pp_loc_node  state "MapInj" region;
    pp_injection pp_binding state value

and pp_tuple_expr state {value; _} =
  let exprs          = Utils.nsepseq_to_list value.inside in
  let length         = List.length exprs in
  let apply len rank = pp_expr (state#pad len rank)
  in List.iteri (apply length) exprs

and pp_string_expr state = function
  Cat {value; region} ->
    pp_loc_node state "Cat" region;
    pp_expr     (state#pad 2 0) value.arg1;
    pp_expr     (state#pad 2 1) value.arg2;
| String s ->
    pp_node   state "String";
    pp_string (state#pad 1 0) s
| Verbatim v ->
    pp_node   state "Verbatim";
    pp_verbatim (state#pad 1 0) v

and pp_annotated state (expr, _, t_expr) =
  pp_expr      (state#pad 2 0) expr;
  pp_type_expr (state#pad 2 1) t_expr

and pp_bin_op node region state op =
  pp_loc_node state node region;
  pp_expr     (state#pad 2 0) op.arg1;
  pp_expr     (state#pad 2 1) op.arg2

and pp_type_annot state rank = function
  None -> rank
| Some (_, e) -> pp_type_expr state e; rank+1
