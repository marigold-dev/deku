open Ast_core.Misc
module Core = Typesystem.Core

module I = Ast_core
module T = Ast_core
module O = Core

type constraints = O.type_constraint list


let rec type_expression_to_type_value : T.type_expression -> O.type_value = fun te ->
  match te.type_content with
  | T_variable tv -> T.Reasons.wrap (Todo "wrap: from source code maybe?") @@ T.P_variable tv
  | T_singleton _ -> failwith "what about singleton ?"
  | T_sum {fields ; layout=_} ->
    let aux ({associated_type;michelson_annotation;decl_pos}:T.row_element) : T.row_value =
       {associated_value = type_expression_to_type_value associated_type;michelson_annotation;decl_pos} in
    p_row C_variant @@ T.LMap.map aux fields
  | T_record {fields ; layout=_} ->
    let aux ({associated_type;michelson_annotation;decl_pos}:T.row_element) : T.row_value =
       {associated_value = type_expression_to_type_value associated_type;michelson_annotation;decl_pos} in
    p_row C_record @@ T.LMap.map aux fields
  | T_arrow {type1;type2} ->
    p_constant C_arrow @@ List.map ~f:type_expression_to_type_value [ type1 ; type2 ]
  | T_module_accessor {module_name=_; element} ->
    type_expression_to_type_value element
  | T_app {type_operator;arguments} -> (
    let open Stage_common.Constant in
    let (csttag, args) = Option.value_exn (* This will be removed later *)
    T.(match (Var.to_name type_operator , arguments) with
      | ( s , [] ) when String.equal s unit_name -> Some (C_unit , [])
      | ( s , [] ) when String.equal s never_name -> Some (C_never , [])
      | ( s , [] ) when String.equal s string_name-> Some (C_string , [])
      | ( s , [] ) when String.equal s nat_name-> Some (C_nat , [])
      | ( s , [] ) when String.equal s tez_name-> Some (C_mutez , [])
      | ( s , [] ) when String.equal s timestamp_name-> Some (C_timestamp , [])
      | ( s , [] ) when String.equal s int_name-> Some (C_int , [])
      | ( s , [] ) when String.equal s address_name-> Some (C_address , [])
      | ( s , [] ) when String.equal s bytes_name-> Some (C_bytes , [])
      | ( s , [] ) when String.equal s key_hash_name-> Some (C_key_hash , [])
      | ( s , [] ) when String.equal s key_name-> Some (C_key , [])
      | ( s , [] ) when String.equal s signature_name-> Some (C_signature , [])
      | ( s , [] ) when String.equal s operation_name-> Some (C_operation , [])
      | ( s , [] ) when String.equal s chain_id_name-> Some (C_chain_id , [])
      | ( s , [o] ) when String.equal s option_name -> Some (C_option, [o])
      | ( s , [p] ) when String.equal s set_name -> Some (C_set, [p])
      | ( s , [ k ; v ]) when String.equal s map_name -> Some (C_map, [k;v])
      | ( s , [ k ; v ]) when String.equal s big_map_name -> Some (C_big_map, [k;v])
      | ( s , [ k ; v ]) when String.equal s map_or_big_map_name -> Some (C_map, [k;v])
      | ( s , [l] ) when String.equal s list_name -> Some (C_list, [l])
      | ( s , [c] ) when String.equal s contract_name -> Some (C_contract, [c])
      | ( _ , _ ) -> None
      )
    in
    p_constant csttag @@ List.map ~f:type_expression_to_type_value args
  )
  | T_abstraction { ty_binder ; kind = _ ; type_ } -> (
    let body = type_expression_to_type_value type_ in
    p_for_all ty_binder.wrap_content [] body
  )
  | T_for_all { ty_binder ; kind = _ ; type_ } -> (
    let body = type_expression_to_type_value type_ in
    p_for_all ty_binder.wrap_content [] body
  )

let variable : I.expression_variable -> T.type_expression -> (constraints * T.type_variable) = fun name expr ->
  let pattern = type_expression_to_type_value expr in
  let type_name = Core.fresh_for_expr_var name in
  let aval = T.Reasons.(wrap (Todo "wrap: variable: whole") (T.P_variable type_name)) in
  [{ c = C_equation { aval ; bval = pattern } ; reason = "wrap: variable" }] , type_name

let literal : string -> T.type_expression -> (constraints * T.type_variable) = fun name t ->
  let pattern = type_expression_to_type_value t in
  let type_name = Core.fresh_type_variable ~name:("literal_" ^ name) () in
  let aval = T.Reasons.(wrap (Todo "wrap: literal: whole") (T.P_variable type_name)) in
  [{ c = C_equation { aval ; bval = pattern } ; reason = "wrap: literal" }] , type_name

(* TODO : move to common *)
let lmap_of_tuple lst =
  let aux i e = (i+1,(T.Label (string_of_int i),e)) in
  T.LMap.of_list @@ snd @@ List.fold_map ~f:aux ~init:0 lst

(* This is pretty much a wrapper for an n-ary function. *)
(* TODO: change working of constant in ligo *)
let constant : I.constant' -> O.type_value -> T.type_expression list -> (constraints * T.type_variable) =
  fun name f args ->
  let whole_expr = Core.fresh_type_variable ~name:(Format.asprintf "capp_%a" I.PP.constant' name) () in
  let args'     = lmap_of_tuple @@ List.mapi ~f:(fun i arg -> ({associated_value = type_expression_to_type_value arg ; michelson_annotation = None; decl_pos = i}: T.row_value)) args in
  let args_tuple = p_row C_record args' in
  [
      c_equation f (p_constant C_arrow ([args_tuple ; (T.Reasons.wrap (Todo "wrap: constant: whole") (T.P_variable whole_expr))])) "wrap: constant: as declared for built-in"
  ] , whole_expr

(* TODO : change type of lambda *)
let lambda
    : T.type_expression ->
      T.type_expression option ->
      T.type_expression option ->
      T.type_expression ->
      (constraints * T.type_variable) =
  fun fresh arg output result ->
  let whole_expr = Core.fresh_type_variable ~name:"lambda" () in
  let unification_arg = T.( Reasons.wrap (Todo "wrap: lambda: arg") @@ P_variable (Core.fresh_type_variable ~name:"args" ()) ) in
  let unification_output = T.( Reasons.wrap  (Todo "wrap: lambda: whole") @@ P_variable (Core.fresh_type_variable ~name:"result" ()) ) in
  let result' = type_expression_to_type_value result in
  let arg'  = match arg with
      None -> []
    | Some arg -> [c_equation unification_arg (type_expression_to_type_value arg) "wrap: lambda: arg annot"] in
  let output'  = match output with
      None -> []
    | Some output -> [c_equation unification_output (type_expression_to_type_value output) "wrap: lambda: output annot"]
  in
    [
      c_equation unification_output result' "wrap: lambda: result" ;
      c_equation (type_expression_to_type_value fresh) unification_arg "wrap: lambda: arg" ;
      c_equation ((T.Reasons.wrap (Todo "wrap: lambda: whole") @@ T.P_variable whole_expr ))
                 (p_constant C_arrow ([unification_arg ; unification_output]))
                 "wrap: lambda: arrow (whole)"
    ] @ arg' @ output' , whole_expr

let application : I.expr -> T.type_expression -> T.type_expression -> (constraints * T.type_variable) =
  fun name f arg ->
  let pretty =
    match name.expression_content with
      I.E_constant c -> Format.asprintf "app_%a" I.PP.constant' c.cons_name
    | _ -> "app"
  in
  let whole_expr = Core.fresh_type_variable ~name:pretty () in
  let f'   = type_expression_to_type_value f in
  let arg' = type_expression_to_type_value arg in
  [
    c_equation f' (p_constant C_arrow [arg' ; T.Reasons.wrap (Todo "wrap: application: whole") @@ T.P_variable whole_expr ]) "wrap: application: f" ;
  ] , whole_expr

let constructor : T.label -> T.type_expression -> T.type_expression -> T.type_expression -> (constraints * T.type_variable) = fun const t_arg c_arg sum ->
  let t_arg = type_expression_to_type_value t_arg in
  let c_arg = type_expression_to_type_value c_arg in
  let sum = type_expression_to_type_value sum in
  let Label const = const in
  let whole_expr = Core.fresh_type_variable ~name:const () in
  [
    c_equation ( T.Reasons.wrap (Todo "wrap: constructor: whole") @@ T.P_variable whole_expr ) sum "wrap: constructor: whole" ;
    c_equation t_arg c_arg "wrap: construcotr: arg" ;
  ] , whole_expr

let record : T.rows -> (constraints * T.type_variable) = fun {fields;layout} ->
  let record_type = type_expression_to_type_value (T.t_record ?layout fields) in
  let whole_expr = Core.fresh_type_variable ~name:"record" () in
  [c_equation (T.Reasons.wrap (Todo "wrap: record: whole") @@ T.P_variable whole_expr) record_type "wrap: record: whole"] , whole_expr

let access_label ~(base : T.type_expression) ~(label : O.accessor) : (constraints * T.type_variable) =
  let base' = type_expression_to_type_value base in
  let Label l = label in
  let expr_type = Core.fresh_type_variable ~name:("acc_fl_"^l) () in
  [{ c = C_access_label { c_access_label_record_type = base' ; accessor = label ; c_access_label_tvar = expr_type } ; reason = "wrap: access_label" }] , expr_type

let record_update ~(base : T.type_expression) ~(label : O.accessor) (update : T.type_expression) : (constraints * T.type_variable) =
  let base' = type_expression_to_type_value base in
  let update = type_expression_to_type_value update in
  let Label l = label in
  let update_var = Core.fresh_type_variable ~name:("up_fld_"^l) () in
  let whole_expr = Core.fresh_type_variable ~name:"update" () in
  [
    { c = C_access_label { c_access_label_record_type = base' ; accessor = label ; c_access_label_tvar = update_var } ; reason = "wrap: access_label" };
    c_equation update (T.Reasons.wrap (Todo "wrap: record_update: update") @@ T.P_variable update_var) "wrap: record_update: update";
    c_equation base' (T.Reasons.wrap (Todo "wrap: record_update: whole") @@ T.P_variable whole_expr) "wrap: record_update: record (whole)"
  ] , whole_expr

let module_access (expr : T.type_expression) : (constraints * T.type_variable) =
  let expr' = type_expression_to_type_value expr in
  let whole_expr = Core.fresh_type_variable ~name:"module_acces" () in
  [c_equation (T.Reasons.wrap (Todo "wrap: module: whole") @@ T.P_variable whole_expr) expr' "wrap: module: whole"] , whole_expr

let let_in : T.type_variable -> T.type_expression -> T.type_expression option -> T.type_expression -> (constraints * T.type_variable) =
  fun binder rhs rhs_tv_opt result ->
  let rhs'        = type_expression_to_type_value rhs in
  let result'     = type_expression_to_type_value result in
  let unification_binder = T.( Reasons.wrap (Todo "wrap: let_in: binder") @@ P_variable (binder))  in
  let rhs_tv_opt' = match rhs_tv_opt with
      None -> []
    | Some annot -> [c_equation unification_binder (type_expression_to_type_value annot) "wrap: let_in: rhs"] in
  let whole_expr = Core.fresh_type_variable ~name:"let_in" () in
    c_equation result' (T.Reasons.wrap (Todo "wrap: let_in: whole") @@ T.P_variable whole_expr) "wrap: let_in: result (whole)"
  :: c_equation rhs' unification_binder "wrap: lambda: arg"
  :: rhs_tv_opt', whole_expr

let type_in : T.type_expression -> (constraints * T.type_variable) =
  fun result ->
  let result'     = type_expression_to_type_value result in
  let whole_expr = Core.fresh_type_variable () in
  [
    c_equation result' (T.Reasons.wrap (Todo "wrap: type_in: whole") @@ T.P_variable whole_expr) "wrap: type_in: result (whole)"
  ], whole_expr

let mod_in : T.type_expression -> (constraints * T.type_variable) =
  fun result ->
  let result'     = type_expression_to_type_value result in
  let whole_expr = Core.fresh_type_variable () in
  [
    c_equation result' (T.Reasons.wrap (Todo "wrap: mod_in: whole") @@ T.P_variable whole_expr) "wrap: mod_in: result (whole)"
  ], whole_expr

let mod_alias : T.type_expression -> (constraints * T.type_variable) =
  fun result ->
  let result'     = type_expression_to_type_value result in
  let whole_expr = Core.fresh_type_variable () in
  [
    c_equation result' (T.Reasons.wrap (Todo "wrap: mod_alias: whole") @@ T.P_variable whole_expr) "wrap: mod_alias: result (whole)"
  ], whole_expr

let recursive : T.type_variable -> T.type_expression -> (constraints * T.type_variable) =
  fun lambda_type rec_type ->
  let rec_type = type_expression_to_type_value rec_type in
  let whole_expr = Core.fresh_type_variable () in
  [
    c_equation rec_type (T.Reasons.wrap (Todo "wrap: recursive: lambda") @@ T.P_variable lambda_type) "wrap: recursive: lambda_type = rec_type" ;
    c_equation rec_type (T.Reasons.wrap (Todo "wrap: recursive: whole") @@ T.P_variable whole_expr) "wrap: recursive: rec_type (whole)" ;
  ], whole_expr
let pattern_match_var : T.type_expression -> T.type_expression -> constraints = fun t_ascr fresh ->
  let t_ascr = type_expression_to_type_value t_ascr in
  let fresh = type_expression_to_type_value fresh in
  [
    c_equation t_ascr fresh "wrap: match_exp: binder annotation"
  ]
let pattern_match_unit : T.type_expression -> constraints = fun fresh ->
  let fresh = type_expression_to_type_value fresh in
  [
    c_equation (T.p_constant C_unit []) fresh "wrap: match_exp: unit pattern must match against unit"
  ]
let pattern_match_list : T.type_expression -> T.type_expression list -> constraints = fun el_t ts ->
  let el_t = type_expression_to_type_value el_t in
  let ts = List.map ~f:type_expression_to_type_value ts in
  List.map ~f:(fun x -> c_equation el_t x "wrap: match_exp: element types must be identical") ts
let raw_code : T.type_expression -> T.type_expression -> (constraints * T.type_variable) =
  fun type_anno verbatim_string ->
  let type_anno = type_expression_to_type_value type_anno in
  let verbatim_string = type_expression_to_type_value verbatim_string in
  let whole_expr = Core.fresh_type_variable () in
  [
    c_equation type_anno  (T.Reasons.wrap (Todo "wrap: raw_code: whole") @@ T.P_variable whole_expr) "wrap: raw_code: type_anno (whole)" ;
    c_equation verbatim_string (T.p_constant C_string []) "wrap: code is a string";
  ], whole_expr

let annotation : T.type_expression -> T.type_expression -> (constraints * T.type_variable) =
  fun e annot ->
  let e' = type_expression_to_type_value e in
  let annot' = type_expression_to_type_value annot in
  let whole_expr = Core.fresh_type_variable ~name:"annot" () in
  [
    c_equation e' annot' "wrap: annotation: expr type must eq annot" ;
    c_equation e' (T.Reasons.wrap (Todo "wrap: annotation: whole") @@ T.P_variable whole_expr) "wrap: annotation: whole" ;
  ] , whole_expr

let const_decl : T.type_expression -> T.type_expression option -> constraints =
  fun rhs rhs_tv_opt ->
  let rhs'        = type_expression_to_type_value rhs in
  let rhs_tv_opt' = match rhs_tv_opt with
      None -> []
    | Some annot -> [c_equation rhs' (type_expression_to_type_value annot) "wrap: let_in: rhs"] in
  rhs_tv_opt'

let type_decl : unit -> constraints =
  fun () ->
  [
  ]

let mod_decl : unit -> constraints =
  fun () ->
  [
  ]

let mod_al : unit -> constraints =
  fun () ->
  [
  ]

let match_opt :T.type_expression -> T.type_expression -> constraints =
  fun tv tv_some ->
    let tv_some = type_expression_to_type_value tv_some in
    let tv = type_expression_to_type_value tv in
    [
      c_equation tv_some (T.Reasons.wrap (Todo "wrap: match_opt") @@ T.P_constant
        {p_ctor_tag=C_option;p_ctor_args=[tv]} ) "wrap: match_opt"
    ]

let match_lst :T.type_expression -> T.type_expression -> constraints =
  fun elt lst ->
    let lst = type_expression_to_type_value lst in
    let elt = type_expression_to_type_value elt in
    [
      c_equation lst (T.Reasons.wrap (Todo "wrap: match_lst: list of") @@ T.P_constant
        {p_ctor_tag=C_list;p_ctor_args=[elt]} ) "wrap: match_lst: list type must be equal to list of elt type"
    ]

let match_variant : T.label -> case:T.type_expression -> T.type_value -> T.type_expression -> T.type_value -> constraints =
  fun cons ~case case_env t t_env ->
    let t = type_expression_to_type_value t in
    let t_var = Core.fresh_type_variable () in
    let case  = type_expression_to_type_value case in
  [
    c_equation t_env t "wrap: match_variant t env";
    c_equation case_env case "wrap: match_variant env";
    (* bellow will go (after constraints simplification) *)
    c_equation case (T.Reasons.wrap (Todo "wrap: match_variant") @@ T.P_variable t_var) "wrap: match_variant";
    { c = C_access_label { c_access_label_record_type = t ; accessor = cons ; c_access_label_tvar = t_var } ; reason = "wrap: match_variant" }
  ]

let match_record : T.type_expression T.label_map -> T.type_expression -> constraints =
  fun row t ->
    let t = type_expression_to_type_value t in
    let row = T.LMap.map (fun v ->
      let v = type_expression_to_type_value v in
      T.{associated_value=v;michelson_annotation=None;decl_pos=0}
      ) row in
  [
    c_equation t (T.p_row T.C_record row) "wrap: match_record"
  ]

let match_cases : T.type_expression list -> T.type_expression list -> T.type_expression -> (constraints * T.type_variable) =
  fun body_ts pattern_ts matchee_t ->
    let match_whole = Core.fresh_type_variable ~name:("match_whole") () in
    let match_whole_expr = (T.Reasons.wrap (Todo "wrap: match_whole") @@ T.P_variable match_whole) in
    let body_ts = List.map ~f:type_expression_to_type_value body_ts in
    let body_cs = List.map ~f:(fun tbody -> c_equation tbody match_whole_expr "wrap: match_whole body") body_ts in

    let pattern_ts = List.map ~f:type_expression_to_type_value pattern_ts in
    let matchee_t = type_expression_to_type_value matchee_t in
    let pattern_cs = List.map ~f:(fun tpat -> c_equation tpat matchee_t  "wrap: matchee") pattern_ts in
    body_cs @ pattern_cs , match_whole
