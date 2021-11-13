open Types
module S = Ast_sugar
open Stage_common.Constant

let make_t ?(loc = Location.generated) ?sugar type_content = {type_content; location=loc; sugar }
let make_e ?(loc = Location.generated) ?sugar expression_content = {  
  expression_content ;
  location = loc ;
  sugar ;
  }
let t_variable   ?loc ?sugar t  : type_expression = make_t ?loc ?sugar (T_variable t)

let t_constant ?loc ?sugar type_operator arguments : type_expression =
  make_t ?loc ?sugar (T_app {type_operator=Var.of_name type_operator;arguments})
let t_abstraction ?loc ?sugar ty_binder kind type_ =
  make_t ?loc ?sugar (T_abstraction {ty_binder ; kind ; type_})
let t_for_all ?loc ?sugar ty_binder kind type_ =
  make_t ?loc ?sugar (T_for_all {ty_binder ; kind ; type_})

  (*X_name here should be replaced by X_injection*)
let t_signature  ?loc ?sugar () : type_expression = t_constant ?loc ?sugar signature_name []
let t_chain_id   ?loc ?sugar () : type_expression = t_constant ?loc ?sugar chain_id_name []
let t_string     ?loc ?sugar () : type_expression = t_constant ?loc ?sugar string_name []
let t_bytes      ?loc ?sugar () : type_expression = t_constant ?loc ?sugar bytes_name []
let t_key        ?loc ?sugar () : type_expression = t_constant ?loc ?sugar key_name []
let t_key_hash   ?loc ?sugar () : type_expression = t_constant ?loc ?sugar key_hash_name []
let t_int        ?loc ?sugar () : type_expression = t_constant ?loc ?sugar int_name []
let t_address    ?loc ?sugar () : type_expression = t_constant ?loc ?sugar address_name []
let t_operation  ?loc ?sugar () : type_expression = t_constant ?loc ?sugar operation_name []
let t_nat        ?loc ?sugar () : type_expression = t_constant ?loc ?sugar nat_name []
let t_mutez      ?loc ?sugar () : type_expression = t_constant ?loc ?sugar tez_name []
let t_timestamp  ?loc ?sugar () : type_expression = t_constant ?loc ?sugar timestamp_name []
let t_unit       ?loc ?sugar () : type_expression = t_constant ?loc ?sugar unit_name []
let t_bls12_381_g1 ?loc ?sugar () : type_expression = t_constant ?loc ?sugar bls12_381_g1_name []
let t_bls12_381_g2 ?loc ?sugar () : type_expression = t_constant ?loc ?sugar bls12_381_g2_name []
let t_bls12_381_fr ?loc ?sugar () : type_expression = t_constant ?loc ?sugar bls12_381_fr_name []
let t_never      ?loc ?sugar () : type_expression = t_constant ?loc ?sugar never_name []

let t_abstraction1 ?loc ?sugar name kind : type_expression = 
  let ty_binder = Location.wrap @@ Var.fresh () in
  let type_ = t_constant name [t_variable ty_binder.wrap_content] in
  t_abstraction ?loc ?sugar ty_binder kind type_
let t_abstraction2 ?loc ?sugar name kind_l kind_r : type_expression = 
  let ty_binder_l = Location.wrap @@ Var.fresh () in
  let ty_binder_r = Location.wrap @@ Var.fresh () in
  let type_ = t_constant name [t_variable ty_binder_l.wrap_content ; t_variable ty_binder_r.wrap_content] in
  t_abstraction ?loc ?sugar ty_binder_l kind_l (t_abstraction ?loc ty_binder_r kind_r type_)

let t_option         ?loc ?sugar o   : type_expression = t_constant ?loc ?sugar option_name [o]
let t_list           ?loc ?sugar t   : type_expression = t_constant ?loc ?sugar list_name [t]
let t_set            ?loc ?sugar t   : type_expression = t_constant ?loc ?sugar set_name [t]
let t_contract       ?loc ?sugar t   : type_expression = t_constant ?loc ?sugar contract_name [t]
let t_ticket         ?loc ?sugar t   : type_expression = t_constant ?loc ?sugar ticket_name [t]
let t_map            ?loc ?sugar k v : type_expression = t_constant ?loc ?sugar map_name [ k ; v ]
let t_big_map        ?loc ?sugar k v : type_expression = t_constant ?loc ?sugar big_map_name [ k ; v ]
let t_map_or_big_map ?loc ?sugar k v : type_expression = t_constant ?loc ?sugar map_or_big_map_name [ k ; v ]


let t_record ?loc ?sugar ?layout fields  : type_expression = make_t ?loc ?sugar @@ T_record {fields;layout}
let default_layout = L_tree
let make_t_ez_record ?loc ?sugar ?layout (lst:(string * type_expression) list) : type_expression =
  let lst = List.mapi ~f:(fun i (x,y) -> (Label x, ({associated_type=y;michelson_annotation=None;decl_pos=i} : row_element)) ) lst in
  let map = LMap.of_list lst in
  t_record ?loc ?sugar ?layout map

let ez_t_record ?loc ?sugar ?(layout=default_layout) lst : type_expression =
  let m = LMap.of_list lst in
  t_record ?loc ?sugar ~layout m
let t_pair ?loc ?sugar a b : type_expression =
  ez_t_record ?loc ?sugar [
    (Label "0",{associated_type=a;michelson_annotation=None ; decl_pos = 0}) ;
    (Label "1",{associated_type=b;michelson_annotation=None ; decl_pos = 1}) ]

let t_sum ?loc ?sugar ?layout fields : type_expression = make_t ?loc ?sugar @@ T_sum {fields;layout}
let t_sum_ez ?loc ?sugar ?layout (lst:(string * type_expression) list) : type_expression =
  let lst = List.mapi ~f:(fun i (x,y) -> (Label x, ({associated_type=y;michelson_annotation=None;decl_pos=i}:row_element)) ) lst in
  let map = LMap.of_list lst in
  t_sum ?loc ?sugar ?layout map

let t_bool ?loc ?sugar ()       : type_expression = t_sum_ez ?loc ?sugar
  [("True", t_unit ());("False", t_unit ())]

let t_function ?loc ?sugar param result : type_expression = make_t ?loc ?sugar (T_arrow {type1=param; type2=result})
let t_shallow_closure ?loc ?sugar param result: type_expression = make_t ?loc ?sugar (T_arrow {type1=param; type2=result})

let get_type' (x:type_expression) = x.type_content
let get_expression (x:expression) = x.expression_content

let get_lambda e : (_,_) lambda option = match e.expression_content with
  | E_lambda l -> Some l
  | _ -> None

let get_t_bool (t:type_expression) : unit option = match t.type_content with
  | t when (compare t (t_bool ()).type_content) = 0-> Some ()
  | _ -> None


let tuple_of_record (m: _ LMap.t) =
  let aux i =
    let opt = LMap.find_opt (Label (string_of_int i)) m in
    Option.bind ~f: (fun opt -> Some (opt,i+1)) opt
  in
  let l = Base.Sequence.to_list @@ Base.Sequence.unfold ~init:0 ~f:aux in
  List.map ~f:(fun {associated_type;_} -> associated_type) l


let get_t_tuple (t:type_expression) : type_expression list option = match t.type_content with
  | T_record record -> Some (tuple_of_record record.fields)
  | _ -> None

let get_t_pair (t:type_expression) : (type_expression * type_expression) option = match t.type_content with
  | T_record m ->
      let lst = tuple_of_record m.fields in
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

let get_t_sum (t:type_expression) : rows option = match t.type_content with
  | T_sum m -> Some m
  | _ -> None

let get_t_sum_exn (t:type_expression) : rows = match t.type_content with
  | T_sum m -> m
  | _ -> raise (Failure ("Internal error: broken invariant at " ^ __LOC__))

let get_t_record (t:type_expression) : rows option = match t.type_content with
  | T_record m -> Some m
  | _ -> None



let e_record map : expression = make_e @@ E_record map
let ez_e_record (lst : (label * expression) list) : expression =
  let aux prev (k, v) = LMap.add k v prev in
  let map = List.fold_left ~f:aux ~init:LMap.empty lst in
  e_record map

let e_var       ?loc ?sugar n  : expression = make_e ?loc ?sugar @@ E_variable (Location.wrap ?loc (Var.of_name n))
let e_literal   ?loc ?sugar l  : expression = make_e ?loc ?sugar @@ E_literal l
let e_unit      ?loc ?sugar () : expression = make_e ?loc ?sugar @@ E_literal (Literal_unit)
let e_int       ?loc ?sugar n  : expression = make_e ?loc ?sugar @@ E_literal (Literal_int n)
let e_nat       ?loc ?sugar n  : expression = make_e ?loc ?sugar @@ E_literal (Literal_nat n)
let e_timestamp ?loc ?sugar n  : expression = make_e ?loc ?sugar @@ E_literal (Literal_timestamp n)
let e_string    ?loc ?sugar s  : expression = make_e ?loc ?sugar @@ E_literal (Literal_string s)
let e_address   ?loc ?sugar s  : expression = make_e ?loc ?sugar @@ E_literal (Literal_address s)
let e_mutez     ?loc ?sugar s  : expression = make_e ?loc ?sugar @@ E_literal (Literal_mutez s)
let e_signature ?loc ?sugar s  : expression = make_e ?loc ?sugar @@ E_literal (Literal_signature s)
let e_key       ?loc ?sugar s  : expression = make_e ?loc ?sugar @@ E_literal (Literal_key s)
let e_key_hash  ?loc ?sugar s  : expression = make_e ?loc ?sugar @@ E_literal (Literal_key_hash s)
let e_chain_id  ?loc ?sugar s  : expression = make_e ?loc ?sugar @@ E_literal (Literal_chain_id s)
let e_operation ?loc ?sugar s  : expression = make_e ?loc ?sugar @@ E_literal (Literal_operation s)
let e'_bytes b : expression_content =
  let bytes = Hex.to_bytes (`Hex b) in
  E_literal (Literal_bytes bytes)
let e_bytes_hex ?loc ?sugar b : expression =
  let e' = e'_bytes b in
  make_e ?loc ?sugar e'
let e_bytes_raw ?loc ?sugar (b: bytes) : expression =
  make_e ?loc ?sugar @@ E_literal (Literal_bytes b)
let e_bytes_string ?loc ?sugar (s: string) : expression =
  make_e ?loc ?sugar @@ E_literal (Literal_bytes (Hex.to_bytes (Hex.of_string s)))
let e_some       ?loc ?sugar s        : expression = make_e ?loc ?sugar @@ E_constant {cons_name = C_SOME; arguments = [s]}
let e_none       ?loc ?sugar ()       : expression = make_e ?loc ?sugar @@ E_constant {cons_name = C_NONE; arguments = []}
let e_string_cat ?loc ?sugar sl sr    : expression = make_e ?loc ?sugar @@ E_constant {cons_name = C_CONCAT; arguments = [sl ; sr ]}
let e_map_add    ?loc ?sugar k v old  : expression = make_e ?loc ?sugar @@ E_constant {cons_name = C_MAP_ADD; arguments = [k ; v ; old]}

let e_constant    ?loc ?sugar name lst             = make_e ?loc ?sugar @@ E_constant {cons_name=name ; arguments = lst}
let e_variable v            : expression = make_e @@ E_variable v
let e_application lamb args : expression = make_e @@ E_application {lamb;args}
let e_lambda      ?loc ?sugar binder output_type result            = make_e ?loc ?sugar @@ E_lambda {binder; output_type; result ;  }
let e_lambda_ez   ?loc ?sugar var ?ascr ?const_or_var output_type result         = e_lambda ?loc ?sugar {var;ascr;attributes={const_or_var}} output_type result
let e_recursive   ?loc ?sugar fun_name fun_type lambda             = make_e ?loc ?sugar @@ E_recursive {fun_name; fun_type; lambda}
let e_let_in      ?loc ?sugar let_binder rhs let_result attr = make_e ?loc ?sugar @@ E_let_in { let_binder ; rhs ; let_result; attr }
let e_let_in_ez   ?loc ?sugar var ?ascr ?const_or_var inline rhs let_result = e_let_in ?loc ?sugar {var;ascr;attributes={const_or_var}} rhs let_result inline
let e_type_in type_binder rhs let_result = make_e @@ E_type_in { type_binder ; rhs ; let_result }
let e_mod_in      ?loc ?sugar module_binder rhs let_result         = make_e ?loc ?sugar @@ E_mod_in { module_binder ; rhs ; let_result }
let e_mod_alias  ?loc ?sugar  alias binders result = make_e ?loc ?sugar @@ E_mod_alias { alias ; binders ; result }
let e_raw_code    ?loc ?sugar language code                       = make_e ?loc ?sugar @@ E_raw_code {language; code}

let e_constructor constructor element: expression = make_e @@ E_constructor {constructor;element}
let e_matching    ?loc ?sugar a b : expression = make_e ?loc ?sugar @@ E_matching {matchee=a;cases=b}


let e_record          ?loc ?sugar map                = make_e ?loc ?sugar @@ E_record map
let e_record_accessor ?loc ?sugar record path        = make_e ?loc ?sugar @@ E_record_accessor ({record; path} : _ record_accessor)
let e_record_update   ?loc ?sugar record path update = make_e ?loc ?sugar @@ E_record_update ({record; path; update} : _ record_update)

let e_module_accessor ?loc ?sugar module_name element = make_e ?loc ?sugar @@ E_module_accessor {module_name;element}

let e_ascription ?loc ?sugar anno_expr type_annotation  : expression = make_e ?loc ?sugar @@ E_ascription {anno_expr;type_annotation}

let e_bool b : expression =
  if b then
    e_constructor (Label "True") (e_ascription (e_unit ())(t_unit()))
  else
    e_constructor (Label "False") (e_ascription (e_unit ())(t_unit()))

let get_e_int (t:expression) =
  match t.expression_content with
  | E_literal (Literal_int n) -> Some n
  | _ -> None

let get_e_string (t:expression) =
  match t.expression_content with
  | E_literal (Literal_string s) -> Some (Ligo_string.extract s)
  | _ -> None

let get_e_verbatim (t:expression) =
  match t.expression_content with
    E_literal (Literal_string (Verbatim v)) -> Some v
  | _ -> None

let get_e_unit (t:expression) =
  match t.expression_content with
  | E_literal (Literal_unit) -> Some ()
  | _ -> None

let get_e_bool (t:expression) =
  match t.expression_content with
  | E_constructor {constructor=Label name;element} when (String.equal name "True")
    && element.expression_content = (e_unit ()).expression_content ->
      Some true
  | E_constructor {constructor=Label name;element} when (String.equal name "False")
    && element.expression_content = (e_unit ()).expression_content ->
      Some false
  | _ -> None


let get_e_pair = fun t ->
  match t with
  | E_record r -> (
  let lst = LMap.to_kv_list_rev r in
    match lst with
    | [(Label "O",a);(Label "1",b)]
    | [(Label "1",b);(Label "0",a)] ->
        Some (a , b)
    | _ -> None
    )
  | _ -> None

let get_e_list = fun t ->
  let rec aux t =
    match t with
      E_constant {cons_name=C_CONS;arguments=[key;lst]} ->
        let lst = aux lst.expression_content in
        (Some key)::(lst)
    | E_constant {cons_name=C_LIST_EMPTY;arguments=[]} ->
        []
    | _ -> [None]
  in
  Option.all @@ aux t

let get_e_tuple = fun t ->
  match t with
  | E_record r -> Some (List.map ~f:snd @@ Helpers.tuple_of_record r)
  | _ -> None

let get_e_record = fun t ->
  match t.expression_content with
  | E_record record -> Some record
  | _ -> None

let get_e_record_accessor = fun t ->
  match t.expression_content with
  | E_record_accessor {record; path} -> Some (record, path)
  | _ -> None

let get_declaration_by_name : module_ -> string -> declaration option = fun (p) name ->
  let aux : declaration -> bool = fun declaration ->
    match declaration with
    | Declaration_constant { name = name'; binder = _ ; expr=_ ; attr=_ } ->
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
    match LMap.find_opt label record.fields with
    | None -> None
    | Some row_element -> Some row_element.associated_type

let get_e_ascription = fun a ->
  match a with
  | E_ascription {anno_expr; type_annotation} -> Some (anno_expr,type_annotation)
  | _ -> None

(* Same as get_e_pair *)
let extract_pair : expression -> (expression * expression) option = fun e ->
  match e.expression_content with
  | E_record r -> (
  let lst = LMap.to_kv_list_rev r in
    match lst with
    | [(Label "O",a);(Label "1",b)]
    | [(Label "1",b);(Label "0",a)] ->
      Some (a , b)
    | _ -> None
    )
  | _ -> None

let extract_record : expression -> (label * expression) list option = fun e ->
  match e.expression_content with
  | E_record lst -> Some (LMap.to_kv_list lst)
  | _ -> None

let extract_map : expression -> (expression * expression) list option = fun e ->
  let rec aux e =
    match e.expression_content with
      E_constant {cons_name=C_UPDATE|C_MAP_ADD; arguments=[k;v;map]} ->
        let map = aux map in
        (Some (k,v))::map
    | E_constant {cons_name=C_MAP_EMPTY|C_BIG_MAP_EMPTY; arguments=[]} -> []
    | _ -> [None]
  in
  Option.all @@ aux e

let make_c_constructor_simpl ?(reason_constr_simpl="") id_constructor_simpl original_id tv c_tag tv_list = {
  reason_constr_simpl ;
  id_constructor_simpl = ConstraintIdentifier.T (Int64.of_int id_constructor_simpl) ;
  original_id = Option.map ~f:(fun i -> ConstraintIdentifier.T (Int64.of_int i)) original_id;
  tv ;
  c_tag;
  tv_list
}

let make_c_row_simpl ?(reason_row_simpl="") id_row_simpl original_id tv r_tag tv_map_as_lst : c_row_simpl = { 
  reason_row_simpl ;
  id_row_simpl = ConstraintIdentifier.T (Int64.of_int id_row_simpl) ;
  original_id = Option.map ~f:(fun i -> ConstraintIdentifier.T (Int64.of_int i)) original_id;
  tv;
  r_tag;
  tv_map = LMap.of_list @@ List.mapi ~f:(fun i (k,v) -> (k,{associated_variable=v;michelson_annotation=None;decl_pos=i})) tv_map_as_lst ;
}


(* TODO: remove this () argument, it is just here to make sure that
   the ~bound and ~constraints arguments are given (while adding the
   fields to the record, we need to make sure they're supplied
   everywhere) *)
let make_c_typeclass_simpl ?(reason_typeclass_simpl="") ~bound ~constraints () id_typeclass_simpl original_id args tc : c_typeclass_simpl =
  {
    tc_bound = bound;
    tc_constraints = constraints;
    reason_typeclass_simpl ;
    id_typeclass_simpl = ConstraintIdentifier.T (Int64.of_int id_typeclass_simpl) ;
    original_id = Option.map ~f:(fun i -> ConstraintIdentifier.T (Int64.of_int i)) original_id;
    tc ;
    args ;
  }

let make_constructor_or ?(reason_constr_simpl = "") id_constructor_simpl original_id tv c_tag tv_list =
  `Constructor (make_c_constructor_simpl ~reason_constr_simpl id_constructor_simpl original_id tv c_tag tv_list)

let make_row_or ?(reason_row_simpl = "") id_row_simpl original_id tv r_tag tv_map_as_lst : constructor_or_row =
  `Row (make_c_row_simpl ~reason_row_simpl id_row_simpl original_id tv r_tag tv_map_as_lst)

let make_alias ?(reason_alias_simpl="") a b :  type_constraint_simpl = SC_Alias {
  reason_alias_simpl ;
  a ;
  b ;
}

let make_sc_alias ?(reason_alias_simpl="") a b : type_constraint_simpl =
  SC_Alias {
    reason_alias_simpl ;
    a ;
    b ;
  }
let make_sc_constructor ?(reason_constr_simpl="") id_constructor_simpl original_id tv c_tag tv_list : type_constraint_simpl =
  SC_Constructor (make_c_constructor_simpl ~reason_constr_simpl id_constructor_simpl original_id tv c_tag tv_list)
let make_sc_row ?(reason_row_simpl="") id_row_simpl original_id tv r_tag tv_map_as_lst : type_constraint_simpl =
  SC_Row (make_c_row_simpl ~reason_row_simpl id_row_simpl original_id tv r_tag tv_map_as_lst)
(* TODO: remove this () argument, it is just here to make sure that
   the ~bound and ~constraints arguments are given (while adding the
   fields to the record, we need to make sure they're supplied
   everywhere) *)
let make_sc_typeclass ?(reason_typeclass_simpl="") ~bound ~constraints () (tc : typeclass) (args : type_variable list) =
  SC_Typeclass {
    tc_bound = bound;
    tc_constraints = constraints;
    reason_typeclass_simpl ;
    id_typeclass_simpl = ConstraintIdentifier.fresh () ;
    original_id = None ;
    tc ;
    args ;
  }
let make_sc_poly ?(reason_poly_simpl="") (tv:type_variable) (forall:p_forall) =
  SC_Poly {
    reason_poly_simpl ;
    id_poly_simpl = ConstraintIdentifier.fresh () ;
    original_id = None ;
    tv ;
    forall ;
  }
let make_sc_access_label ?(reason_access_label_simpl="") (tv:type_variable) ~(record_type:type_variable) (label:label) =
  SC_Access_label {
    reason_access_label_simpl ;
    id_access_label_simpl = ConstraintIdentifier.fresh () ;
    tv ;
    record_type ;
    label ;
  }
