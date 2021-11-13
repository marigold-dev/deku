open Types
module S = Ast_core
open Stage_common.Constant

let make_t_orig_var ?(loc = Location.generated) type_content core orig_var = {type_content; location=loc; type_meta = core ; orig_var}
let make_t ?(loc = Location.generated) type_content core = {type_content; location=loc; type_meta = core ; orig_var = None}
let make_e ?(location = Location.generated) expression_content type_expression = {
  expression_content ;
  type_expression ;
  location ;
  }
let t_variable   ?loc ?core t  : type_expression = make_t ?loc (T_variable t) core
let t_abstraction ?loc ?core ty_binder kind type_ =
  make_t ?loc (T_abstraction {ty_binder ; kind ; type_}) core
let t_for_all ?loc ?core ty_binder kind type_ =
  make_t ?loc (T_for_all {ty_binder ; kind ; type_}) core

let t_constant ?loc ?core injection parameters : type_expression =
  make_t ?loc (T_constant {language=Stage_common.Backends.michelson; injection = Ligo_string.verbatim injection ; parameters}) core

  (*X_name here should be replaced by X_injection*)
let t_signature  ?loc ?core () : type_expression = t_constant ?loc ?core signature_name []
let t_chain_id   ?loc ?core () : type_expression = t_constant ?loc ?core chain_id_name []
let t_string     ?loc ?core () : type_expression = t_constant ?loc ?core string_name []
let t_bytes      ?loc ?core () : type_expression = t_constant ?loc ?core bytes_name []
let t_key        ?loc ?core () : type_expression = t_constant ?loc ?core key_name []
let t_key_hash   ?loc ?core () : type_expression = t_constant ?loc ?core key_hash_name []
let t_int        ?loc ?core () : type_expression = t_constant ?loc ?core int_name []
let t_address    ?loc ?core () : type_expression = t_constant ?loc ?core address_name []
let t_operation  ?loc ?core () : type_expression = t_constant ?loc ?core operation_name []
let t_nat        ?loc ?core () : type_expression = t_constant ?loc ?core nat_name []
let t_mutez      ?loc ?core () : type_expression = t_constant ?loc ?core tez_name []
let t_timestamp  ?loc ?core () : type_expression = t_constant ?loc ?core timestamp_name []
let t_unit       ?loc ?core () : type_expression = t_constant ?loc ?core unit_name []
let t_bls12_381_g1 ?loc ?core () : type_expression = t_constant ?loc ?core bls12_381_g1_name []
let t_bls12_381_g2 ?loc ?core () : type_expression = t_constant ?loc ?core bls12_381_g2_name []
let t_bls12_381_fr ?loc ?core () : type_expression = t_constant ?loc ?core bls12_381_fr_name []
let t_never       ?loc ?core () : type_expression = t_constant ?loc ?core never_name []
let t_pvss_key ?loc ?core () : type_expression = t_constant ?loc ?core pvss_key_name []
let t_baker_hash ?loc ?core () : type_expression = t_constant ?loc ?core baker_hash_name []
let t_chest_key ?loc ?core () : type_expression = t_constant ?loc ?core chest_key_name []
let t_chest ?loc ?core () : type_expression = t_constant ?loc ?core chest_name []

let t_abstraction1 ?loc name kind : type_expression = 
  let ty_binder = Location.wrap @@ Var.fresh () in
  let type_ = t_constant name [t_variable ~core:(Ast_core.t_variable ty_binder.wrap_content) ty_binder.wrap_content] in
  t_abstraction ?loc ty_binder kind type_
let t_abstraction2 ?loc name kind_l kind_r : type_expression = 
  let ty_binder_l = Location.wrap @@ Var.fresh () in
  let ty_binder_r = Location.wrap @@ Var.fresh () in
  let type_ = t_constant name
    [ t_variable ty_binder_l.wrap_content ;
      t_variable ty_binder_r.wrap_content ]
  in
  t_abstraction ?loc ty_binder_l kind_l (t_abstraction ?loc ty_binder_r kind_r type_)

let t_option         ?loc ?core o   : type_expression = t_constant ?loc ?core option_name [o]
let t_list           ?loc ?core t   : type_expression = t_constant ?loc ?core list_name [t]
let t_set            ?loc ?core t   : type_expression = t_constant ?loc ?core set_name [t]
let t_contract       ?loc ?core t   : type_expression = t_constant ?loc ?core contract_name [t]
let t_typed_address  ?loc ?core p s : type_expression = t_constant ?loc ?core typed_address_name [ p ; s ]
let t_mutation       ?loc ?core ()  : type_expression = t_constant ?loc ?core mutation_name []
let t_failure        ?loc ?core ()  : type_expression = t_constant ?loc ?core failure_name []
let t_ticket         ?loc ?core t   : type_expression = t_constant ?loc ?core ticket_name [t]
let t_map            ?loc ?core k v : type_expression = t_constant ?loc ?core map_name [ k ; v ]
let t_big_map        ?loc ?core k v : type_expression = t_constant ?loc ?core big_map_name [ k ; v ]
let t_map_or_big_map ?loc ?core k v : type_expression = t_constant ?loc ?core map_or_big_map_name [ k ; v ]


let t_record ?loc ?core ~layout content  : type_expression = make_t ?loc (T_record {content;layout}) core
let default_layout = L_tree
let make_t_ez_record ?loc ?core ?(layout=default_layout) (lst:(string * type_expression) list) : type_expression =
  let lst = List.mapi ~f:(fun i (x,y) -> (Label x, ({associated_type=y;michelson_annotation=None;decl_pos=i} : row_element)) ) lst in
  let map = LMap.of_list lst in
  t_record ?loc ?core ~layout map

let ez_t_record ?loc ?core ?(layout=default_layout) lst : type_expression =
  let m = LMap.of_list lst in
  t_record ?loc ?core ~layout m
let t_pair ?loc ?core a b : type_expression =
  ez_t_record ?loc ?core [
    (Label "0",{associated_type=a;michelson_annotation=None ; decl_pos = 0}) ;
    (Label "1",{associated_type=b;michelson_annotation=None ; decl_pos = 1}) ]

let t_triplet ?loc ?core a b c : type_expression =
  ez_t_record ?loc ?core [
    (Label "0",{associated_type=a;michelson_annotation=None ; decl_pos = 0}) ;
    (Label "1",{associated_type=b;michelson_annotation=None ; decl_pos = 1}) ;
    (Label "2",{associated_type=c;michelson_annotation=None ; decl_pos = 2}) ]

let t_sum ?loc ?core ~layout content : type_expression = make_t ?loc (T_sum {content;layout}) core
let t_sum_ez ?loc ?core ?(layout=default_layout) (lst:(string * type_expression) list) : type_expression =
  let lst = List.mapi ~f:(fun i (x,y) -> (Label x, ({associated_type=y;michelson_annotation=None;decl_pos=i}:row_element)) ) lst in
  let map = LMap.of_list lst in
  t_sum ?loc ?core ~layout map
let t_bool ?loc ?core ()       : type_expression = t_sum_ez ?loc ?core
  [("True", t_unit ());("False", t_unit ())]

(* types specific to LIGO test framework*)
let t_michelson_code ?loc ?core () : type_expression = t_constant ?loc ?core test_michelson_name []
let t_test_exec_error ?loc ?core () : type_expression = t_sum_ez ?loc ?core
  [ ("Rejected", t_pair (t_michelson_code ()) (t_address ())) ; ("Other" , t_unit ())]
let t_test_exec_result ?loc ?core () : type_expression = t_sum_ez ?loc ?core
  [ ("Success" ,t_unit ()); ("Fail", t_sum_ez [ ("Rejected", t_pair (t_michelson_code ()) (t_address ())) ; ("Other" , t_unit ())])]

let t_function param result ?loc ?s () : type_expression = make_t ?loc (T_arrow {type1=param; type2=result}) s
let t_shallow_closure param result ?loc ?s () : type_expression = make_t ?loc (T_arrow {type1=param; type2=result}) s
let t_chest_opening_result ?loc ?core () : type_expression =
  t_sum_ez ?loc ?core [
    ("Ok_opening", t_bytes ()) ;
    ("Fail_decrypt", t_unit ());
    ("Fail_timelock", t_unit ())
  ]

let get_type_expression (x:expression) = x.type_expression
let get_type' (x:type_expression) = x.type_content
let get_expression (x:expression) = x.expression_content

let get_variable e : expression_variable option = match e.expression_content with
  | E_variable v -> Some v
  | _ -> None

let get_lambda e : lambda option = match e.expression_content with
  | E_lambda l -> Some l
  | _ -> None

let get_lambda_with_type e =
  match (e.expression_content , e.type_expression.type_content) with
  | E_lambda l , T_arrow {type1;type2} -> Some (l , (type1,type2))
  | _ -> None

let get_t_bool (t:type_expression) : unit option = match t.type_content with
  | t when (compare t (t_bool ()).type_content) = 0-> Some ()
  | _ -> None

let get_param_inj (t:type_expression) : (string * Ligo_string.t * type_expression list) option =
  match t.type_content with
  | T_constant {language;injection;parameters} -> Some (language,injection,parameters)
  | _ -> None

let get_t_inj (t:type_expression) (v:string) : (type_expression list) option =
  match t.type_content with
  | T_constant {language=_;injection; parameters} when String.equal (Ligo_string.extract injection) v -> Some parameters
  | _ -> None
let get_t_base_inj (t:type_expression) (v:string) : unit option =
  match get_t_inj t v with
  | Some [] -> Some ()
  | _ -> None
let get_t_unary_inj (t:type_expression) (v:string) : type_expression option =
  match get_t_inj t v with
  | Some [a] -> Some a
  | _ -> None
let get_t_binary_inj (t:type_expression) (v:string) : (type_expression * type_expression) option =
  match get_t_inj t v with
  | Some [a;b] -> Some (a,b)
  | _ -> None
let get_t_int (t:type_expression) : unit option = get_t_base_inj t int_name
let get_t_nat (t:type_expression) : unit option = get_t_base_inj t nat_name
let get_t_unit (t:type_expression) : unit option = get_t_base_inj t unit_name
let get_t_mutez (t:type_expression) : unit option = get_t_base_inj t tez_name
let get_t_timestamp (t:type_expression) : unit option = get_t_base_inj t timestamp_name
let get_t_address (t:type_expression) : unit option = get_t_base_inj t address_name
let get_t_bytes (t:type_expression) : unit option = get_t_base_inj t bytes_name
let get_t_chest (t:type_expression) : unit option = get_t_base_inj t chest_name
let get_t_chest_key (t:type_expression) : unit option = get_t_base_inj t chest_key_name
let get_t_michelson_code (t:type_expression) : unit option = get_t_base_inj t test_michelson_name
let get_t_string (t:type_expression) : unit option = get_t_base_inj t string_name
let get_t_contract (t:type_expression) : type_expression option = get_t_unary_inj t contract_name
let get_t_option (t:type_expression) : type_expression option = get_t_unary_inj t option_name
let get_t_list (t:type_expression) : type_expression option = get_t_unary_inj t list_name
let get_t_set (t:type_expression) : type_expression option = get_t_unary_inj t set_name
let get_t_ticket (t:type_expression) : type_expression option = get_t_unary_inj t ticket_name
let get_t_key (t:type_expression) : unit option = get_t_base_inj t key_name
let get_t_signature (t:type_expression) : unit option = get_t_base_inj t signature_name
let get_t_key_hash (t:type_expression) : unit option = get_t_base_inj t key_hash_name
let get_t_sapling_state (t:type_expression) : type_expression option = get_t_unary_inj t sapling_state_name
let get_t_sapling_transaction (t:type_expression) : type_expression option = get_t_unary_inj t sapling_transaction_name

let tuple_of_record (m: _ LMap.t) =
  let aux i =
    let opt = LMap.find_opt (Label (string_of_int i)) m in
    Option.bind ~f: (fun opt -> Some (opt,i+1)) opt
  in
  let l = Base.Sequence.to_list @@ Base.Sequence.unfold ~init:0 ~f:aux in
  List.map ~f:(fun {associated_type;_} -> associated_type) l


let get_t_tuple (t:type_expression) : type_expression list option = match t.type_content with
  | T_record record -> Some (tuple_of_record record.content)
  | _ -> None

let get_t_pair (t:type_expression) : (type_expression * type_expression) option = match t.type_content with
  | T_record m ->
      let lst = tuple_of_record m.content in
      ( match List.(length lst = 2) with
        | true -> Some (List.(nth_exn lst 0 , nth_exn lst 1))
        | false -> None
      )
  | _ -> None

let get_t_function (t:type_expression) : (type_expression * type_expression) option = match t.type_content with
  | T_arrow {type1;type2} -> Some (type1,type2)
  | _ -> None

let get_t_function_exn t = match get_t_function t with
  | Some x -> x
  | None -> raise (Failure ("Internal error: broken invariant at " ^ __LOC__))

let get_t_for_all (t : type_expression) : (type_variable Location.wrap * type_expression) option =
  match t.type_content with
  | T_for_all { ty_binder ; type_ ; _ } -> Some (ty_binder, type_)
  | _ -> None

let get_t_sum (t:type_expression) : rows option = match t.type_content with
  | T_sum m -> Some m
  | _ -> None

let get_t_sum_exn (t:type_expression) : rows = match t.type_content with
  | T_sum m -> m
  | _ -> raise (Failure ("Internal error: broken invariant at " ^ __LOC__))

let get_t_record (t:type_expression) : rows option = match t.type_content with
  | T_record m -> Some m
  | _ -> None

let get_t_record_exn (t:type_expression) : rows = match t.type_content with
  | T_record m -> m
  | _ -> raise (Failure ("Internal error: broken invariant at " ^ __LOC__))

let get_t_list_exn (t:type_expression) : type_expression =
  match get_t_list t with
  | Some value -> value
  | _ -> raise (Failure ("Internal error: broken invariant at " ^ __LOC__))

let get_t_set_exn (t:type_expression) : type_expression =
  match get_t_set t with
  | Some value -> value
  | _ -> raise (Failure ("Internal error: broken invariant at " ^ __LOC__))

let get_t_map (t:type_expression) : (type_expression * type_expression) option =
  match t.type_content with
  | T_constant {language=_;injection; parameters = [k;v]} when String.equal (Ligo_string.extract injection) map_name -> Some (k,v)
  | T_constant {language=_;injection; parameters = [k;v]} when String.equal (Ligo_string.extract injection) map_or_big_map_name -> Some (k,v)
  | _ -> None

let get_t_typed_address (t:type_expression) : (type_expression * type_expression) option =
  match t.type_content with
  | T_constant {language=_;injection; parameters = [k;v]} when String.equal (Ligo_string.extract injection) typed_address_name -> Some (k,v)
  | _ -> None

let get_t_big_map (t:type_expression) : (type_expression * type_expression) option =
  match t.type_content with
  | T_constant {language=_;injection; parameters = [k;v]} when String.equal (Ligo_string.extract injection) big_map_name -> Some (k,v)
  | T_constant {language=_;injection; parameters = [k;v]} when String.equal (Ligo_string.extract injection) map_or_big_map_name -> Some (k,v)
  | _ -> None

let get_t_map_key : type_expression -> type_expression option = fun t ->
  match get_t_map t with
  | Some (key,_) -> Some key
  | None -> None

let get_t_map_value : type_expression -> type_expression option = fun t ->
  match get_t_map t with
  | Some (_,value) -> Some value
  | None -> None

let get_t_big_map_key : type_expression -> type_expression option = fun t ->
  match get_t_big_map t with
  | Some (key,_) -> Some key
  | None -> None

let get_t_big_map_value : type_expression -> type_expression option = fun t ->
  match get_t_big_map t with
  | Some (_,value) -> Some value
  | None -> None

let is_t_map t = Option.is_some (get_t_map t)
let is_t_big_map t = Option.is_some (get_t_big_map t)
let is_t_record t = Option.is_some (get_t_record t)
let is_t_option t = Option.is_some (get_t_option t)
let is_t_sum t = Option.is_some (get_t_sum t)
let is_t_function t = Option.is_some (get_t_function t)

let is_e_matching e = match e.expression_content with | E_matching _ -> true | _ -> false
let assert_t_mutez : type_expression -> unit option = get_t_mutez
let assert_t_key = get_t_key
let assert_t_signature = get_t_signature
let assert_t_key_hash = get_t_key_hash
let assert_t_bytes = get_t_bytes
let assert_t_string = get_t_string
let assert_t_michelson_code = get_t_michelson_code
let assert_t_contract (t:type_expression) : unit option = match get_t_unary_inj t contract_name with
  | Some _ -> Some ()
  | _ -> None

let is_t_list t = Option.is_some (get_t_list t)
let is_t_set t = Option.is_some (get_t_set t)
let is_t_nat t = Option.is_some (get_t_nat t)
let is_t_string t = Option.is_some (get_t_string t)
let is_t_bytes t = Option.is_some (get_t_bytes t)
let is_t_int t = Option.is_some (get_t_int t)
let is_t_bool t = Option.is_some (get_t_bool t)
let is_t_unit t = Option.is_some (get_t_unit t)
let is_t_address t = Option.is_some (get_t_address t)
let is_t_mutez t = Option.is_some (get_t_mutez t)
let is_t_contract t = Option.is_some (get_t_contract t)

let assert_t_list_operation (t : type_expression) : unit option =
  match get_t_list t with
  | Some t' -> get_t_base_inj t' operation_name
  | None -> None

let assert_t_int : type_expression -> unit option = fun t -> get_t_base_inj t int_name
let assert_t_nat : type_expression -> unit option = fun t -> get_t_base_inj t nat_name
let assert_t_bool : type_expression -> unit option = fun v -> get_t_bool v
let assert_t_option : type_expression -> unit option = fun v -> Option.map ~f:(fun _ -> ()) @@ get_t_option v
let assert_t_set : type_expression -> unit option = fun v -> Option.map ~f:(fun _ -> ()) @@ get_t_set v
let assert_t_list : type_expression -> unit option = fun v -> Option.map ~f:(fun _ -> ()) @@ get_t_list v
let assert_t_unit : type_expression -> unit option = fun v -> get_t_unit v

let e_record map : expression_content = E_record map
let ez_e_record (lst : (label * expression) list) : expression_content =
  let aux prev (k, v) = LMap.add k v prev in
  let map = List.fold_left ~f:aux ~init:LMap.empty lst in
  e_record map
let e_some s : expression_content = E_constant {cons_name=C_SOME;arguments=[s]}
let e_none (): expression_content = E_constant {cons_name=C_NONE; arguments=[]}
let e_cons hd tl : expression_content = E_constant {cons_name=C_CONS;arguments=[hd;tl]}
let e_nil (): expression_content = E_constant {cons_name=C_LIST_EMPTY; arguments=[]}
let e_set_add hd tl : expression_content = E_constant {cons_name=C_SET_ADD;arguments=[hd;tl]}
let e_set_empty (): expression_content = E_constant {cons_name=C_SET_EMPTY; arguments=[]}
let e_map_add k v tl : expression_content = E_constant {cons_name=C_MAP_ADD;arguments=[k;v;tl]}
let e_map_empty (): expression_content = E_constant {cons_name=C_MAP_EMPTY; arguments=[]}
let e_big_map_empty (): expression_content = E_constant {cons_name=C_BIG_MAP_EMPTY; arguments=[]}
let e_map_remove k tl : expression_content = E_constant {cons_name=C_MAP_REMOVE; arguments=[k; tl]}
let e_contract_opt v : expression_content = E_constant {cons_name=C_CONTRACT_OPT; arguments=[v]}
let e_contract v : expression_content = E_constant {cons_name=C_CONTRACT; arguments=[v]}
let e_contract_entrypoint e v : expression_content = E_constant {cons_name=C_CONTRACT_ENTRYPOINT; arguments=[e; v]}
let e_contract_entrypoint_opt e v : expression_content = E_constant {cons_name=C_CONTRACT_ENTRYPOINT_OPT; arguments=[e; v]}

let e_failwith e : expression_content = E_constant {cons_name=C_FAILWITH ; arguments=[e]}

let e_literal l : expression_content =     E_literal l
let e_unit () : expression_content =     E_literal (Literal_unit)
let e_int n : expression_content = E_literal (Literal_int n)
let e_nat n : expression_content = E_literal (Literal_nat n)
let e_mutez n : expression_content = E_literal (Literal_mutez n)
let e_string s : expression_content = E_literal (Literal_string s)
let e_bytes s : expression_content = E_literal (Literal_bytes s)
let e_timestamp s : expression_content = E_literal (Literal_timestamp s)
let e_address s : expression_content = E_literal (Literal_address s)
let e_signature s : expression_content = E_literal (Literal_signature s)
let e_key s : expression_content = E_literal (Literal_key s)
let e_key_hash s : expression_content = E_literal (Literal_key_hash s)
let e_chain_id s : expression_content = E_literal (Literal_chain_id s)
let e_operation s : expression_content = E_literal (Literal_operation s)
let e_lambda l : expression_content = E_lambda l
let e_recursive l : expression_content = E_recursive l
let e_pair a b : expression_content = ez_e_record [(Label "0",a);(Label "1", b)]
let e_application lamb args : expression_content = E_application {lamb;args}
let e_raw_code language code : expression_content = E_raw_code { language ; code }
let e_variable v : expression_content = E_variable v
let e_let_in let_binder rhs let_result attr = E_let_in { let_binder ; rhs ; let_result; attr }
let e_mod_in module_binder rhs let_result = E_mod_in { module_binder ; rhs ; let_result }

let e_constructor constructor element: expression_content = E_constructor {constructor;element}

let e_bool b : expression_content =
  if b then
    e_constructor (Label "True") (make_e (e_unit ())(t_unit()))
  else
    e_constructor (Label "False") (make_e (e_unit ())(t_unit()))

let e_a_unit = make_e (e_unit ()) (t_unit ())
let e_a_int n = make_e (e_int n) (t_int ())
let e_a_literal l t = make_e (e_literal l) t
let e_a_nat n = make_e (e_nat n) (t_nat ())
let e_a_mutez n = make_e (e_mutez n) (t_mutez ())
let e_a_timestamp n = make_e (e_timestamp n) (t_timestamp ())
let e_a_key_hash n = make_e (e_key_hash n) (t_key_hash ())
let e_a_bool b = make_e (e_bool b) (t_bool ())
let e_a_string s = make_e (e_string s) (t_string ())
let e_a_bytes b = make_e (e_bytes b) (t_bytes ())
let e_a_address s = make_e (e_address s) (t_address ())
let e_a_pair a b = make_e (e_pair a b)
  (t_pair a.type_expression b.type_expression )
let e_a_constructor c e t = make_e (e_constructor (Label c) e) t
let e_a_some s = make_e (e_some s) (t_constant option_name [s.type_expression])

let e_a_lambda l in_ty out_ty = make_e (e_lambda l) (t_function in_ty out_ty ())
let e_a_recursive l= make_e (e_recursive l) l.fun_type
let e_a_none t = make_e (e_none ()) (t_option t)
let e_a_record ?(layout=default_layout) r = make_e (e_record r) (t_record ~layout
  (LMap.map
    (fun t ->
      let associated_type = get_type_expression t in
      {associated_type ; michelson_annotation=None ; decl_pos = 0} )
    r ))
let e_a_application a b t = make_e (e_application a b) t
let e_a_variable v ty = make_e (e_variable v) ty
let ez_e_a_record ?layout r = make_e (ez_e_record r) (ez_t_record ?layout (List.mapi ~f:(fun i (x, y) -> x, {associated_type = y.type_expression ; michelson_annotation = None ; decl_pos = i}) r))
let e_a_let_in binder expr body attr = make_e (e_let_in binder expr body attr) (get_type_expression body)
let e_a_mod_in module_binder rhs let_result = make_e (e_mod_in module_binder rhs let_result) (get_type_expression let_result)
let e_a_raw_code l c t = make_e (e_raw_code l c) t
let e_a_nil t = make_e (e_nil ()) (t_list t)
let e_a_cons hd tl = make_e (e_cons hd tl) (t_list hd.type_expression)
let e_a_set_empty t = make_e (e_set_empty ()) (t_set t)
let e_a_set_add hd tl = make_e (e_set_add hd tl) (t_set hd.type_expression)
let e_a_map_empty kt vt = make_e (e_map_empty ()) (t_map kt vt)
let e_a_map_add k v tl = make_e (e_map_add k v tl) (t_map k.type_expression v.type_expression)
let e_a_big_map_empty kt vt = make_e (e_big_map_empty ()) (t_big_map kt vt)
let e_a_big_map_add k v tl = make_e (e_map_add k v tl) (t_big_map k.type_expression v.type_expression)
let e_a_big_map_remove k tl = make_e (e_map_remove k tl) tl.type_expression
let e_a_contract_opt a t = make_e (e_contract_opt a) (t_option (t_contract t))
let e_a_contract a t = make_e (e_contract a) (t_contract t)
let e_a_contract_entrypoint e a t = make_e (e_contract_entrypoint e a) (t_contract t)
let e_a_contract_entrypoint_opt e a t = make_e (e_contract_entrypoint_opt e a) (t_option (t_contract t))


let get_a_int (t:expression) =
  match t.expression_content with
  | E_literal (Literal_int n) -> Some n
  | _ -> None

let get_a_string (t:expression) =
  match t.expression_content with
  | E_literal (Literal_string s) -> Some (Ligo_string.extract s)
  | _ -> None

let get_a_verbatim (t:expression) =
  match t.expression_content with
    E_literal (Literal_string (Verbatim v)) -> Some v
  | _ -> None

let get_a_unit (t:expression) =
  match t.expression_content with
  | E_literal (Literal_unit) -> Some ()
  | _ -> None

let get_a_bool (t:expression) =
  match t.expression_content with
  | E_constructor {constructor=Label name;element} when
    (String.equal name "True")
    && element.expression_content = e_unit () ->
      Some true
  | E_constructor {constructor=Label name;element} when
    (String.equal name "False")
    && element.expression_content = e_unit () ->
      Some false
  | _ -> None


let get_a_record = fun t ->
  match t.expression_content with
  | E_record record -> Some record
  | _ -> None

let get_a_record_accessor = fun t ->
  match t.expression_content with
  | E_record_accessor {record; path} -> Some (record, path)
  | _ -> None

let get_declaration_by_name : module_fully_typed -> string -> declaration option = fun (Module_Fully_Typed p) name ->
  let aux : declaration -> bool = fun declaration ->
    match declaration with
    | Declaration_constant { name = name'; binder = _ ; expr = _ ; attr = _ } ->
      (match name' with
       | None -> false
       | Some name' -> String.equal name' name)
    | Declaration_type   _
    | Declaration_module _
    | Module_alias       _ -> false
  in
  List.find ~f:aux @@ List.map ~f:Location.unwrap p

let get_record_field_type (t : type_expression) (label : label) : type_expression option =
  match get_t_record t with
  | None -> None
  | Some record ->
    match LMap.find_opt label record.content with
    | None -> None
    | Some row_element -> Some row_element.associated_type
