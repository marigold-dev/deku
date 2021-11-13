open Types
module Option = Simple_utils.Option

module SMap = Map.String
open Stage_common.Constant

let make_t ?(loc = Location.generated) type_content = {type_content; location=loc}
let t_variable ?loc variable  = make_t ?loc @@ T_variable variable
let t_singleton ?loc x = make_t ?loc @@ T_singleton x
let t_variable_ez ?loc n     : type_expression = t_variable ?loc (Var.of_name n)

let t_app ?loc type_operator arguments : type_expression = make_t ?loc @@ T_app {type_operator ; arguments}

let t_bool ?loc ()        : type_expression = t_variable ?loc v_bool
let t_string ?loc ()      : type_expression = t_variable ?loc v_string
let t_bytes ?loc ()       : type_expression = t_variable ?loc v_bytes
let t_int ?loc ()         : type_expression = t_variable ?loc v_int
let t_operation ?loc ()   : type_expression = t_variable ?loc v_operation
let t_nat ?loc ()         : type_expression = t_variable ?loc v_nat
let t_tez ?loc ()         : type_expression = t_variable ?loc v_tez
let t_unit ?loc ()        : type_expression = t_variable ?loc v_unit
let t_address ?loc ()     : type_expression = t_variable ?loc v_address
let t_signature ?loc ()   : type_expression = t_variable ?loc v_signature
let t_key ?loc ()         : type_expression = t_variable ?loc v_key
let t_key_hash ?loc ()    : type_expression = t_variable ?loc v_key_hash
let t_timestamp ?loc ()   : type_expression = t_variable ?loc v_timestamp
let t_option ?loc o       : type_expression = t_app ?loc v_option [o]
let t_list ?loc t         : type_expression = t_app ?loc v_list [t]


let t_record ?loc record  : type_expression = make_t ?loc @@ T_record record
let t_record_ez_attr ?loc ?(attr=[]) lst =
  let aux i (name, t_expr, attributes) =
    (Label name, {associated_type=t_expr; decl_pos=i; attributes}) in
  let lst = List.mapi ~f:aux lst in
  let fields : ty_expr row_element label_map = LMap.of_list lst
  in t_record ?loc {fields; attributes=attr}
let t_record_ez ?loc ?attr lst =
  let aux (a,b) = a,b,[] in
  let lst' = List.map ~f:aux lst in
  t_record_ez_attr ?loc ?attr lst'

let t_tuple ?loc lst    : type_expression = make_t ?loc @@ T_tuple lst
let t_pair ?loc (a , b) : type_expression = t_tuple ?loc [a; b]

let t_sum ?loc sum : type_expression = make_t ?loc @@ T_sum sum
let t_sum_ez_attr ?loc ?(attr=[]) lst =
  let aux i (name, t_expr, attributes) =
    (Label name, {associated_type=t_expr; decl_pos=i; attributes}) in
  let lst = List.mapi ~f:aux lst in
  let fields : ty_expr row_element label_map = LMap.of_list lst
  in t_sum ?loc {fields; attributes=attr}

let t_annoted ?loc ty str : type_expression = make_t ?loc @@ T_annoted (ty, str)
let t_module_accessor ?loc module_name element = make_t ?loc @@ T_module_accessor {module_name;element}

let t_function ?loc type1 type2  : type_expression = make_t ?loc @@ T_arrow {type1; type2}
let t_abstraction ?loc ty_binder kind type_ : type_expression = make_t ?loc @@ T_abstraction { ty_binder ; kind ; type_ }
let t_for_all ?loc ty_binder kind type_ : type_expression   = make_t ?loc @@ T_for_all { ty_binder ; kind ; type_ }
let t_map ?loc key value                  : type_expression = t_app ?loc v_map [key; value]
let t_big_map ?loc key value              : type_expression = t_app ?loc v_big_map [key; value]
let t_set ?loc key                        : type_expression = t_app ?loc v_set [key]
let t_contract ?loc contract              : type_expression = t_app ?loc v_contract [contract]
let t_michelson_or ?loc l l_ann r r_ann   : type_expression = t_app ?loc v_michelson_or [t_annoted l l_ann; t_annoted r r_ann]
let t_michelson_pair ?loc l l_ann r r_ann : type_expression = t_app ?loc v_michelson_pair [t_annoted l l_ann; t_annoted r r_ann]
let t_sapling_state ?loc a                : type_expression = t_app ?loc v_sapling_state [a]
let t_sapling_transaction ?loc a                : type_expression = t_app ?loc v_sapling_trasaction [a]

let get_t_annoted = fun te ->
  match te.type_content with
    T_annoted (te, lst) -> Some (te,lst)
  | _ -> None

let make_e ?(loc = Location.generated) expression_content =
  let location = loc in
  { expression_content; location }

let e_literal ?loc l : expression = make_e ?loc @@ E_literal l
let e_unit ?loc () : expression = make_e ?loc @@ E_literal (Literal_unit)
let e_int_z ?loc n : expression = make_e ?loc @@ E_literal (Literal_int n)
let e_int ?loc n : expression = e_int_z ?loc @@ Z.of_int n
let e_nat_z ?loc n : expression = make_e ?loc @@ E_literal (Literal_nat n)
let e_nat ?loc n : expression = e_nat_z ?loc @@ Z.of_int n
let e_timestamp_z ?loc n : expression = make_e ?loc @@ E_literal (Literal_timestamp n)
let e_timestamp ?loc n : expression = e_timestamp_z ?loc @@ Z.of_int n
let e_string ?loc s : expression = make_e ?loc @@ E_literal (Literal_string (Standard s))
let e_verbatim ?loc v : expression = make_e ?loc @@ E_literal (Literal_string (Verbatim v))
let e_address ?loc s : expression = make_e ?loc @@ E_literal (Literal_address s)
let e_mutez_z ?loc s : expression = make_e ?loc @@ E_literal (Literal_mutez s)
let e_mutez ?loc s : expression = e_mutez_z ?loc @@ Z.of_int s
let e_signature ?loc s : expression = make_e ?loc @@ E_literal (Literal_signature s)
let e_key ?loc s : expression = make_e ?loc @@ E_literal (Literal_key s)
let e_key_hash ?loc s : expression = make_e ?loc @@ E_literal (Literal_key_hash s)
let e_chain_id ?loc s : expression = make_e ?loc @@ E_literal (Literal_chain_id s)
let e'_bytes b : expression_content option =
  try
    let bytes = Hex.to_bytes (`Hex b) in
    Some (E_literal (Literal_bytes bytes))
  with _ -> None
let e_bytes_hex_ez ?loc b : expression option =
  match e'_bytes b with
  | Some e' -> Some (make_e ?loc e')
  | None -> None
let e_bytes_raw ?loc (b: bytes) : expression = make_e ?loc @@ E_literal (Literal_bytes b)
let e_bytes_hex ?loc b : expression = e_bytes_raw ?loc @@ Hex.to_bytes b
let e_bytes_string ?loc (s: string) : expression = e_bytes_hex ?loc @@ Hex.of_string s
let e_some ?loc s  : expression = make_e ?loc @@ E_constant {cons_name = Const C_SOME; arguments = [s]}
let e_none ?loc () : expression = make_e ?loc @@ E_constant {cons_name = Const C_NONE; arguments = []}
let e_string_cat ?loc sl sr : expression = make_e ?loc @@ E_constant {cons_name = Const C_CONCAT; arguments = [sl ; sr ]}
let e_map_add ?loc k v old  : expression = make_e ?loc @@ E_constant {cons_name = Const C_MAP_ADD; arguments = [k ; v ; old]}
let e_binop ?loc name a b  = make_e ?loc @@ E_constant {cons_name = name ; arguments = [a ; b]}

let e_constant    ?loc name lst = make_e ?loc @@ E_constant {cons_name=name ; arguments = lst}
let e_variable    ?loc v = make_e ?loc @@ E_variable v
let e_variable_ez ?loc v = e_variable ?loc @@ Location.wrap ?loc (Var.of_name v)
let e_application ?loc a b = make_e ?loc @@ E_application {lamb=a ; args=b}
let e_lambda    ?loc binder output_type result : expression = make_e ?loc @@ E_lambda {binder; output_type; result}
let e_lambda_ez ?loc var ?ascr ?const_or_var output_type result : expression = e_lambda ?loc {var;ascr;attributes={const_or_var}} output_type result
let e_recursive ?loc fun_name fun_type lambda = make_e ?loc @@ E_recursive {fun_name; fun_type; lambda}

(* let e_recursive_ez ?loc fun_name fun_type lambda = e_recursive ?loc (Var.of_name fun_name) fun_type lambda *)
let e_let_in    ?loc let_binder attributes rhs let_result = make_e ?loc @@ E_let_in { let_binder; rhs ; let_result; attributes }
let e_let_in_ez ?loc var ?ascr ?const_or_var attributes rhs let_result = make_e ?loc @@ E_let_in { let_binder={var;ascr;attributes={const_or_var}}; rhs ; let_result; attributes }
(* let e_let_in_ez ?loc binder ascr inline rhs let_result = e_let_in ?loc (Var.of_name binder, ascr) inline rhs let_result *)
let e_type_in   ?loc type_binder rhs let_result = make_e ?loc @@ E_type_in { type_binder; rhs ; let_result}
let e_mod_in    ?loc module_binder rhs let_result = make_e ?loc @@ E_mod_in  { module_binder; rhs ; let_result }
let e_mod_alias ?loc alias binders result = make_e ?loc @@ E_mod_alias { alias; binders ; result }

let e_raw_code ?loc language code = make_e ?loc @@ E_raw_code {language; code}

let e_constructor ?loc s a : expression = make_e ?loc @@ E_constructor { constructor = Label s; element = a}
let e_true  ?loc (): expression = e_constructor ?loc "True"  @@ e_unit ?loc ()
let e_false ?loc (): expression = e_constructor ?loc "False" @@ e_unit ?loc ()
let e_matching ?loc a b : expression = make_e ?loc @@ E_matching {matchee=a;cases=b}
let e_matching_tuple ?loc matchee (binders: _ binder list) body : expression =
  let pv_lst = List.map ~f:(fun (b:_ binder) -> Location.wrap @@ (P_var b)) binders in
  let pattern = Location.wrap @@ P_tuple pv_lst in
  let cases = [ { pattern ; body } ] in
  make_e ?loc @@ E_matching {matchee;cases}
let e_accessor ?loc record path      = make_e ?loc @@ E_accessor {record; path}
let e_update ?loc record path update = make_e ?loc @@ E_update {record; path; update}

let e_annotation ?loc anno_expr ty = make_e ?loc @@ E_ascription {anno_expr; type_annotation = ty}
let e_module_accessor ?loc module_name element = make_e ?loc @@ E_module_accessor {module_name;element}

let e_tuple ?loc lst : expression = make_e ?loc @@ E_tuple lst

let e_pair ?loc a b  : expression = e_tuple ?loc [a;b]
let e_cond ?loc condition then_clause else_clause = make_e ?loc @@ E_cond {condition;then_clause;else_clause}
let e_sequence ?loc expr1 expr2 = make_e ?loc @@ E_sequence {expr1; expr2}
let e_skip ?loc () = make_e ?loc @@ E_skip

let e_list ?loc lst : expression = make_e ?loc @@ E_list lst
let e_set ?loc lst : expression = make_e ?loc @@ E_set lst
let e_map ?loc lst : expression = make_e ?loc @@ E_map lst
let e_big_map ?loc lst : expression = make_e ?loc @@ E_big_map lst

let e_while ?loc cond body = make_e ?loc @@ E_while {cond; body}
let e_for ?loc binder start final incr f_body = make_e ?loc @@ E_for {binder;start;final;incr;f_body}
let e_for_each ?loc fe_binder collection collection_type fe_body = make_e ?loc @@ E_for_each {fe_binder;collection;collection_type;fe_body}

let e_bool ?loc   b : expression =
  if b then e_constructor ?loc "True" (e_unit ())
  else e_constructor ?loc "False" (e_unit ())
let e_record ?loc map = make_e ?loc @@ E_record map
let e_record_ez ?loc (lst : (string * expr) list) : expression =
  let map = List.fold_left ~f:(fun m (x, y) -> LMap.add (Label x) y m) ~init:LMap.empty lst in
  e_record ?loc map

let make_option_typed ?loc e t_opt =
  match t_opt with
  | None -> e
  | Some t -> e_annotation ?loc e t


let e_typed_none ?loc t_opt =
  let type_annotation = t_option t_opt in
  e_annotation ?loc (e_none ?loc ()) type_annotation

let e_typed_list ?loc lst t = e_annotation ?loc (e_list lst) (t_list t)
let e_typed_list_literal ?loc lst t =
  e_annotation ?loc (e_constant (Const C_LIST_LITERAL) lst) (t_list t)

let e_typed_map ?loc lst k v = e_annotation ?loc (e_map lst) (t_map k v)
let e_typed_big_map ?loc lst k v = e_annotation ?loc (e_big_map lst) (t_big_map k v)

let e_typed_set ?loc lst k = e_annotation ?loc (e_set lst) (t_set k)




let e_assign ?loc variable access_path expression = make_e ?loc @@ E_assign {variable;access_path;expression}
let e_assign_ez ?loc variable access_path expression = e_assign ?loc (Location.wrap ?loc @@ Var.of_name variable) access_path expression


let get_e_accessor = fun t ->
  match t with
  | E_accessor {record; path} -> Some (record , path)
  | _ -> None

let assert_e_accessor = fun t ->
  match get_e_accessor t with
  | None -> None
  | Some _ -> Some ()

let get_e_pair = fun t ->
  match t with
  | E_tuple [a ; b] -> Some (a , b)
  | _ -> None

let get_e_list = fun t ->
  match t with
  | E_list lst -> Some lst
  | _ -> None

let get_e_tuple = fun t ->
  match t with
  | E_tuple t -> Some t
  | _ -> None

let get_e_lambda = fun e ->
  match e with
    E_lambda e -> Some e
  | _ -> None

(* Same as get_e_pair *)
let extract_pair : expression -> (expression * expression) option = fun e ->
  match e.expression_content with
  | E_tuple [a;b] -> Some (a,b)
  | _ -> None

let extract_list : expression -> expression list option = fun e ->
  match e.expression_content with
  | E_list lst -> Some lst
  | _ -> None

let extract_record : expression -> (label * expression) list option = fun e ->
  match e.expression_content with
  | E_record lst -> Some (LMap.to_kv_list lst)
  | _ -> None

let extract_map : expression -> (expression * expression) list option = fun e ->
  match e.expression_content with
  | E_map lst -> Some lst
  | _ -> None

(* This function takes a type `∀ v1 ... vn . t` into the pair `([ v1 ; .. ; vn ] , t)` *)
let destruct_for_alls (t : type_expression) =
  let rec destruct_for_alls type_vars (t : type_expression) = match t.type_content with
    | T_for_all { ty_binder ; type_ ; _ } ->
       destruct_for_alls (Location.unwrap ty_binder :: type_vars) type_
    | _ -> (type_vars, t)
  in destruct_for_alls [] t
