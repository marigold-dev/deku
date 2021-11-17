open Types
module Option = Simple_utils.Option

module SMap = Map.String
open Stage_common.Constant

let make_t ?(loc = Location.generated) type_content = {type_content; location=loc}


let tuple_to_record lst =
  let aux (i,acc) el = (i+1,(string_of_int i, el)::acc) in
  let (_, lst ) = List.fold_left ~f:aux ~init:(0,[]) lst in
  lst

let t_variable ?loc variable : type_expression = make_t ?loc @@ T_variable variable
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
let t_record_ez ?loc lst =
  let lst = List.map ~f:(fun (k, v) -> (Label k, v)) lst in
  let fields = LMap.of_list lst in
  make_t ?loc @@ T_record {fields ; attributes=[]}
let t_record ?loc m  : type_expression =
  let lst = SMap.to_kv_list_rev m in
  t_record_ez ?loc lst

let t_pair ?loc (a , b) : type_expression = t_record_ez ?loc [
                                                          ("0",{associated_type=a ; attributes=[] (* TODO *); decl_pos=0}) ;
                                                          ("1",{associated_type=b ; attributes=[] (* TODO *); decl_pos=1})]
let t_tuple ?loc lst    : type_expression = t_record_ez ?loc (tuple_to_record lst)

let t_sum_ez ?loc (lst:((string * ty_expr row_element) list)) : type_expression =
  let aux prev (k, v) = LMap.add (Label k) v prev in
  let fields = List.fold_left ~f:aux ~init:LMap.empty lst in
  make_t ?loc @@ T_sum {fields ; attributes=[]}
let t_sum ?loc m : type_expression =
  let lst = SMap.to_kv_list_rev m in
  t_sum_ez ?loc lst

let t_function ?loc type1 type2  : type_expression = make_t ?loc @@ T_arrow {type1; type2}
let t_map ?loc key value                  : type_expression = t_app ?loc v_map [key; value]
let t_big_map ?loc key value              : type_expression = t_app ?loc v_big_map [key; value]
let t_set ?loc key                        : type_expression = t_app ?loc v_set [key]
let t_contract ?loc contract              : type_expression = t_app ?loc v_contract [contract]


let make_e ?(loc = Location.generated) expression_content =
  let location = loc in
  { expression_content; location }

let e_literal ?loc l : expression = make_e ?loc @@ E_literal l
let e_unit ?loc () : expression = make_e ?loc @@ E_literal (Literal_unit)
let e_int ?loc n : expression = make_e ?loc @@ E_literal (Literal_int n)
let e_nat ?loc n : expression = make_e ?loc @@ E_literal (Literal_nat n)
let e_timestamp ?loc n : expression = make_e ?loc @@ E_literal (Literal_timestamp n)
let e_string ?loc s : expression = make_e ?loc @@ E_literal (Literal_string s)
let e_address ?loc s : expression = make_e ?loc @@ E_literal (Literal_address s)
let e_mutez ?loc s : expression = make_e ?loc @@ E_literal (Literal_mutez s)
let e_signature ?loc s : expression = make_e ?loc @@ E_literal (Literal_signature s)
let e_key ?loc s : expression = make_e ?loc @@ E_literal (Literal_key s)
let e_key_hash ?loc s : expression = make_e ?loc @@ E_literal (Literal_key_hash s)
let e_chain_id ?loc s : expression = make_e ?loc @@ E_literal (Literal_chain_id s)
let e'_bytes b : expression_content =
  let bytes = Hex.to_bytes (`Hex b) in
  E_literal (Literal_bytes bytes)
let e_bytes_hex ?loc b : expression =
  let e' = e'_bytes b in
  make_e ?loc e'
let e_bytes_raw ?loc (b: bytes) : expression =
  make_e ?loc @@ E_literal (Literal_bytes b)
let e_bytes_string ?loc (s: string) : expression =
  make_e ?loc @@ E_literal (Literal_bytes (Hex.to_bytes (Hex.of_string s)))
let e_some ?loc s  : expression = make_e ?loc @@ E_constant {cons_name = C_SOME; arguments = [s]}
let e_none ?loc () : expression = make_e ?loc @@ E_constant {cons_name = C_NONE; arguments = []}

let e_constant ?loc name lst = make_e ?loc @@ E_constant {cons_name=name ; arguments = lst}
let e_variable ?loc v = make_e ?loc @@ E_variable v
let e_application ?loc a b = make_e ?loc @@ E_application {lamb=a ; args=b}
let e_lambda    ?loc binder output_type result : expression = make_e ?loc @@ E_lambda {binder; output_type; result}
let e_lambda_ez ?loc var ?ascr ?const_or_var output_type result : expression = e_lambda ?loc {var;ascr;attributes={const_or_var}} output_type result
let e_recursive ?loc fun_name fun_type lambda = make_e ?loc @@ E_recursive {fun_name; fun_type; lambda}
let e_let_in    ?loc let_binder mut attributes rhs let_result = make_e ?loc @@ E_let_in { let_binder ; rhs ; let_result; attributes; mut }
let e_let_in_ez ?loc var ?ascr ?const_or_var mut attributes rhs let_result = e_let_in ?loc {var;ascr;attributes={const_or_var}} mut attributes rhs let_result
let e_raw_code ?loc language code = make_e ?loc @@ E_raw_code {language; code}

let e_constructor ?loc s a : expression = make_e ?loc @@ E_constructor { constructor = s; element = a}
let e_matching ?loc a b : expression = make_e ?loc @@ E_matching {matchee=a;cases=b}

let e_record ?loc map : expression = make_e ?loc @@ E_record map
let e_accessor ?loc record path = make_e ?loc @@ E_accessor {record; path}
let e_update ?loc record path update = make_e ?loc @@ E_update {record; path; update}

let e_annotation ?loc anno_expr ty = make_e ?loc @@ E_ascription {anno_expr; type_annotation = ty}

let e_tuple ?loc lst : expression = make_e ?loc @@ E_tuple lst
let e_pair ?loc a b  : expression = e_tuple ?loc [a;b]

let e_cond ?loc condition then_clause else_clause = make_e ?loc @@ E_cond {condition;then_clause;else_clause}
let e_sequence ?loc expr1 expr2 = make_e ?loc @@ E_sequence {expr1; expr2}
let e_skip ?loc () = make_e ?loc @@ E_skip

let e_list ?loc lst : expression = make_e ?loc @@ E_list lst
let e_set ?loc lst : expression = make_e ?loc @@ E_set lst
let e_map ?loc lst : expression = make_e ?loc @@ E_map lst
let e_big_map ?loc lst : expression = make_e ?loc @@ E_big_map lst

let e_bool ?loc   b : expression =
  if b then e_constructor ?loc (Label "True") (e_unit ())
  else e_constructor ?loc (Label "False") (e_unit ())
let make_option_typed ?loc e t_opt =
  match t_opt with
  | None -> e
  | Some t -> e_annotation ?loc e t


let e_typed_none ?loc t_opt =
  let type_annotation = t_option t_opt in
  e_annotation ?loc (e_none ?loc ()) type_annotation

let e_typed_list ?loc lst t =
  e_annotation ?loc (e_list lst) (t_list t)

let e_typed_map ?loc lst k v = e_annotation ?loc (e_map lst) (t_map k v)
let e_typed_big_map ?loc lst k v = e_annotation ?loc (e_big_map lst) (t_big_map k v)

let e_typed_set ?loc lst k = e_annotation ?loc (e_set lst) (t_set k)



let get_e_accessor = fun t ->
  match t with
  | E_accessor {record; path} -> Some (record, path)
  | _ -> None

let assert_e_accessor = fun t ->
  match get_e_accessor t with
  | Some _ -> Some ()
  | None -> None

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

(* Same as get_e_pair *)
let extract_pair : expression -> (expression * expression) option = fun e ->
  match e.expression_content with
  | E_tuple [a;b] -> Some (a,b)
  | _ -> None

let extract_list : expression -> (expression list) option = fun e ->
  match e.expression_content with
  | E_list lst -> Some lst
  | _ -> None

let extract_record : expression -> ((label * expression) list) option = fun e ->
  match e.expression_content with
  | E_record lst -> Some (LMap.to_kv_list lst)
  | _ -> None

let extract_map : expression -> ((expression * expression) list) option = fun e ->
  match e.expression_content with
  | E_map lst -> Some lst
  | _ -> None
