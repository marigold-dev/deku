(* The compiler is a function that takes as input the Typed AST, and outputs expressions in a language that is basically a Michelson with named variables and first-class-environments.

   For more info, see back-end.md: https://gitlab.com/ligolang/ligo/blob/dev/gitlab-pages/docs/contributors/big-picture/back-end.md *)

open Trace
module Errors = Errors
open Errors

module AST = Ast_typed
module Append_tree = Tree.Append
open AST.Combinators
open Mini_c

module SMap = Map.Make(String)

let temp_unwrap_loc = Location.unwrap
let temp_unwrap_loc_list = List.map ~f:Location.unwrap

let compile_constant' : AST.constant' -> constant' = function
  | C_INT -> C_INT
  | C_UNIT -> C_UNIT
  | C_NEVER -> C_NEVER
  | C_NIL -> C_NIL
  | C_NOW -> C_NOW
  | C_IS_NAT -> C_IS_NAT
  | C_SOME -> C_SOME
  | C_NONE -> C_NONE
  | C_UNOPT -> C_UNOPT
  | C_UNOPT_WITH_ERROR -> C_UNOPT_WITH_ERROR
  | C_ASSERTION -> C_ASSERTION
  | C_ASSERTION_WITH_ERROR -> C_ASSERTION_WITH_ERROR
  | C_ASSERT_SOME -> C_ASSERT_SOME
  | C_ASSERT_SOME_WITH_ERROR -> C_ASSERT_SOME_WITH_ERROR
  | C_ASSERT_INFERRED -> C_ASSERT_INFERRED
  | C_FAILWITH -> C_FAILWITH
  | C_UPDATE -> C_UPDATE
  (* Loops *)
  | C_ITER -> C_ITER
  | C_FOLD_WHILE -> C_FOLD_WHILE
  | C_FOLD_CONTINUE -> C_FOLD_CONTINUE
  | C_FOLD_STOP -> C_FOLD_STOP
  | C_LOOP_LEFT -> C_LOOP_LEFT
  | C_LOOP_CONTINUE -> C_LOOP_CONTINUE
  | C_LOOP_STOP -> C_LOOP_STOP
  | C_FOLD -> C_FOLD
  | C_FOLD_LEFT -> C_FOLD_LEFT
  | C_FOLD_RIGHT -> C_FOLD_RIGHT
  (* MATH *)
  | C_NEG -> C_NEG
  | C_ABS -> C_ABS
  | C_ADD -> C_ADD
  | C_SUB -> C_SUB
  | C_MUL -> C_MUL
  | C_EDIV -> C_EDIV
  | C_DIV -> C_DIV
  | C_MOD -> C_MOD
  (* LOGIC *)
  | C_NOT -> C_NOT
  | C_AND -> C_AND
  | C_OR -> C_OR
  | C_XOR -> C_XOR
  | C_LSL -> C_LSL
  | C_LSR -> C_LSR
  (* COMPARATOR *)
  | C_EQ -> C_EQ
  | C_NEQ -> C_NEQ
  | C_LT -> C_LT
  | C_GT -> C_GT
  | C_LE -> C_LE
  | C_GE -> C_GE
  (* Bytes/ String *)
  | C_SIZE -> C_SIZE
  | C_CONCAT -> C_CONCAT
  | C_SLICE -> C_SLICE
  | C_BYTES_PACK -> C_BYTES_PACK
  | C_BYTES_UNPACK -> C_BYTES_UNPACK
  | C_CONS -> C_CONS
  (* Pair *)
  | C_PAIR -> C_PAIR
  | C_CAR -> C_CAR
  | C_CDR -> C_CDR
  | C_LEFT -> C_LEFT
  | C_RIGHT -> C_RIGHT
  | C_TRUE -> C_TRUE
  | C_FALSE -> C_FALSE
  (* Set *)
  | C_SET_EMPTY -> C_SET_EMPTY
  | C_SET_LITERAL -> C_SET_LITERAL
  | C_SET_ADD -> C_SET_ADD
  | C_SET_REMOVE -> C_SET_REMOVE
  | C_SET_ITER -> C_SET_ITER
  | C_SET_FOLD -> C_SET_FOLD
  | C_SET_FOLD_DESC -> C_SET_FOLD_DESC
  | C_SET_MEM -> C_SET_MEM
  | C_SET_UPDATE -> C_SET_UPDATE
  (* List *)
  | C_LIST_EMPTY -> C_LIST_EMPTY
  | C_LIST_LITERAL -> C_LIST_LITERAL
  | C_LIST_ITER -> C_LIST_ITER
  | C_LIST_MAP -> C_LIST_MAP
  | C_LIST_FOLD -> C_LIST_FOLD
  | C_LIST_FOLD_LEFT -> C_LIST_FOLD_LEFT
  | C_LIST_FOLD_RIGHT -> C_LIST_FOLD_RIGHT
  | C_LIST_HEAD_OPT -> C_LIST_HEAD_OPT
  | C_LIST_TAIL_OPT -> C_LIST_TAIL_OPT
  (* Maps *)
  | C_MAP -> C_MAP
  | C_MAP_EMPTY -> C_MAP_EMPTY
  | C_MAP_LITERAL -> C_MAP_LITERAL
  | C_MAP_GET -> C_MAP_GET
  | C_MAP_GET_FORCE -> C_MAP_GET_FORCE
  | C_MAP_ADD -> C_MAP_ADD
  | C_MAP_REMOVE -> C_MAP_REMOVE
  | C_MAP_UPDATE -> C_MAP_UPDATE
  | C_MAP_ITER -> C_MAP_ITER
  | C_MAP_MAP -> C_MAP_MAP
  | C_MAP_FOLD -> C_MAP_FOLD
  | C_MAP_MEM -> C_MAP_MEM
  | C_MAP_FIND -> C_MAP_FIND
  | C_MAP_FIND_OPT -> C_MAP_FIND_OPT
  (* Big Maps *)
  | C_BIG_MAP -> C_BIG_MAP
  | C_BIG_MAP_EMPTY -> C_BIG_MAP_EMPTY
  | C_BIG_MAP_LITERAL -> C_BIG_MAP_LITERAL
  (* Crypto *)
  | C_SHA256 -> C_SHA256
  | C_SHA512 -> C_SHA512
  | C_BLAKE2b -> C_BLAKE2b
  | C_HASH -> C_HASH
  | C_HASH_KEY -> C_HASH_KEY
  | C_CHECK_SIGNATURE -> C_CHECK_SIGNATURE
  | C_CHAIN_ID -> C_CHAIN_ID
  (* Blockchain *)
  | C_CALL -> C_CALL
  | C_CONTRACT -> C_CONTRACT
  | C_CONTRACT_WITH_ERROR -> C_CONTRACT_WITH_ERROR
  | C_CONTRACT_OPT -> C_CONTRACT_OPT
  | C_CONTRACT_ENTRYPOINT -> C_CONTRACT_ENTRYPOINT
  | C_CONTRACT_ENTRYPOINT_OPT -> C_CONTRACT_ENTRYPOINT_OPT
  | C_AMOUNT -> C_AMOUNT
  | C_BALANCE -> C_BALANCE
  | C_SOURCE -> C_SOURCE
  | C_SENDER -> C_SENDER
  | C_ADDRESS -> C_ADDRESS
  | C_SELF -> C_SELF
  | C_SELF_ADDRESS -> C_SELF_ADDRESS
  | C_IMPLICIT_ACCOUNT -> C_IMPLICIT_ACCOUNT
  | C_SET_DELEGATE -> C_SET_DELEGATE
  | C_CREATE_CONTRACT -> C_CREATE_CONTRACT
  | C_SHA3 -> C_SHA3
  | C_KECCAK -> C_KECCAK
  | C_LEVEL -> C_LEVEL
  | C_VOTING_POWER -> C_VOTING_POWER 
  | C_TOTAL_VOTING_POWER -> C_TOTAL_VOTING_POWER
  | C_TICKET -> C_TICKET
  | C_READ_TICKET -> C_READ_TICKET
  | C_SPLIT_TICKET -> C_SPLIT_TICKET
  | C_JOIN_TICKET -> C_JOIN_TICKET
  | C_PAIRING_CHECK -> C_PAIRING_CHECK
  | C_MAP_GET_AND_UPDATE -> C_MAP_GET_AND_UPDATE
  | C_BIG_MAP_GET_AND_UPDATE -> C_BIG_MAP_GET_AND_UPDATE
  | C_SAPLING_EMPTY_STATE -> C_SAPLING_EMPTY_STATE
  | C_SAPLING_VERIFY_UPDATE -> C_SAPLING_VERIFY_UPDATE
  | C_POLYMORPHIC_ADD -> C_POLYMORPHIC_ADD
  | C_OPEN_CHEST -> C_OPEN_CHEST
  | C_VIEW -> C_VIEW
  | (   C_TEST_ORIGINATE
      | C_TEST_SET_NOW
      | C_TEST_SET_SOURCE
      | C_TEST_SET_BAKER
      | C_TEST_EXTERNAL_CALL_TO_CONTRACT
      | C_TEST_EXTERNAL_CALL_TO_CONTRACT_EXN
      | C_TEST_EXTERNAL_CALL_TO_ADDRESS
      | C_TEST_EXTERNAL_CALL_TO_ADDRESS_EXN
      | C_TEST_GET_STORAGE
      | C_TEST_GET_STORAGE_OF_ADDRESS
      | C_TEST_GET_BALANCE
      | C_TEST_MICHELSON_EQUAL
      | C_TEST_LOG
      | C_TEST_GET_NTH_BS
      | C_TEST_STATE_RESET
      | C_TEST_BOOTSTRAP_CONTRACT
      | C_TEST_NTH_BOOTSTRAP_CONTRACT
      | C_TEST_LAST_ORIGINATIONS
      | C_TEST_RUN
      | C_TEST_EVAL
      | C_TEST_COMPILE_CONTRACT
      | C_TEST_TO_CONTRACT
      | C_TEST_TO_ENTRYPOINT
      | C_TEST_TO_TYPED_ADDRESS
      | C_TEST_SET_BIG_MAP
      | C_TEST_NTH_BOOTSTRAP_TYPED_ADDRESS
      | C_TEST_ORIGINATE_FROM_FILE
      | C_TEST_COMPILE_META_VALUE
      | C_TEST_MUTATE_COUNT
      | C_TEST_MUTATE_VALUE
      | C_TEST_MUTATION_TEST
      | C_TEST_MUTATION_TEST_ALL
      | C_TEST_CAST_ADDRESS
      | C_TEST_CREATE_CHEST
      | C_TEST_CREATE_CHEST_KEY
      | C_TEST_SAVE_MUTATION) as c ->
    failwith (Format.asprintf "%a is only available for LIGO interpreter" PP.constant c)

let rec compile_type ~raise (t:AST.type_expression) : type_expression =
  let compile_type = compile_type ~raise in
  let return tc = Expression.make_t ~loc:t.location @@ tc in
  match t.type_content with
  | T_variable (name) -> raise.raise @@ no_type_variable @@ name
  | t when (AST.Compare.type_content t (t_bool ()).type_content) = 0-> return (T_base TB_bool)
  | T_constant {language ; injection ; parameters} -> (
    let open Stage_common.Constant in
    let () = Assert.assert_true ~raise (corner_case ~loc:__LOC__ "unsupported language") @@ String.equal language Stage_common.Backends.michelson in
    match Ligo_string.extract injection , parameters with
    | (i, []) when String.equal i bool_name -> return (T_base TB_bool)
    | (i, []) when String.equal i unit_name -> return (T_base TB_unit)
    | (i, []) when String.equal i int_name -> return (T_base TB_int)
    | (i, []) when String.equal i nat_name -> return (T_base TB_nat)
    | (i, []) when String.equal i timestamp_name -> return (T_base TB_timestamp)
    | (i, []) when String.equal i tez_name -> return (T_base TB_mutez)
    | (i, []) when String.equal i string_name -> return (T_base TB_string)
    | (i, []) when String.equal i bytes_name -> return (T_base TB_bytes)
    | (i, []) when String.equal i address_name -> return (T_base TB_address)
    | (i, []) when String.equal i operation_name -> return (T_base TB_operation)
    | (i, []) when String.equal i key_name -> return (T_base TB_key)
    | (i, []) when String.equal i key_hash_name -> return (T_base TB_key_hash)
    | (i, []) when String.equal i chain_id_name -> return (T_base TB_chain_id)
    | (i, []) when String.equal i signature_name -> return (T_base TB_signature)
    | (i, []) when String.equal i baker_hash_name -> return (T_base TB_baker_hash)
    | (i, []) when String.equal i pvss_key_name -> return (T_base TB_pvss_key)
    | (i, []) when String.equal i chest_name -> return (T_base TB_chest)
    | (i, []) when String.equal i chest_key_name -> return (T_base TB_chest_key)
    | (i, []) when String.equal i baker_operation_name -> return (T_base TB_baker_operation)
    | (i, []) when String.equal i bls12_381_g1_name -> return (T_base TB_bls12_381_g1)
    | (i, []) when String.equal i bls12_381_g2_name -> return (T_base TB_bls12_381_g2)
    | (i, []) when String.equal i bls12_381_fr_name -> return (T_base TB_bls12_381_fr)
    | (i, []) when String.equal i never_name -> return (T_base TB_never)
    | (i, [x]) when String.equal i ticket_name ->
      let x' = compile_type x in
      return (T_ticket x')
    | (i, [x]) when String.equal i sapling_transaction_name -> (
      match x.type_content with
      | AST.T_singleton (Literal_int x') -> return (T_sapling_transaction x')
      | _ -> failwith "wrong sapling_transaction"
    )
    | (i, [x]) when String.equal i sapling_state_name -> (
      match x.type_content with
      | AST.T_singleton (Literal_int x') -> return (T_sapling_state x')
      | _ -> failwith "wrong sapling_state"
    )
    | (i, [x]) when String.equal i contract_name ->
      let x' = compile_type x in
      return (T_contract x')
    | (i, [o]) when String.equal i option_name ->
      let o' = compile_type o in
      return (T_option o')
    | (i, [k;v]) when String.equal i map_name ->
      let kv' = Pair.map ~f:compile_type (k, v) in
      return (T_map kv')
    | (i, [k; v]) when String.equal i big_map_name ->
      let kv' = Pair.map ~f:compile_type (k, v) in
      return (T_big_map kv')
    | (i, _) when String.equal i map_or_big_map_name ->
      raise.raise @@ corner_case ~loc:"spilling" "TC_map_or_big_map should have been resolved before spilling"
    | (i, [t]) when String.equal i list_name ->
      let t' = compile_type t in
      return (T_list t')
    | (i, [t]) when String.equal i set_name ->
      let t' = compile_type t in
      return (T_set t')
    | _ -> raise.raise @@ corner_case ~loc:__LOC__ "wrong constant"
  )
  | T_sum { content = m ; layout } -> (
      let open Ast_typed.Helpers in
      match is_michelson_or m with
      | Some (a , b) -> (
          let aux (x : AST.row_element) =
            let t = compile_type x.associated_type in
            let annot = remove_empty_annotation x.michelson_annotation in
            (annot , t)
          in
          let a' = aux a in
          let b' = aux b in
          return @@ T_or (a' , b')
        )
      | None -> Layout.t_sum ~raise ~layout return compile_type m
    )
  | T_record { content = m ; layout } -> (
      let open Ast_typed.Helpers in
      match is_michelson_pair m with
      | Some (a , b) -> (
          let aux (x : AST.row_element) =
            let t = compile_type x.associated_type in
            let annot = remove_empty_annotation x.michelson_annotation in
            (annot , t)
          in
          let a' = aux a in
          let b' = aux b in
          let t = T_tuple [a'; b'] in
          return t
        )
      | None -> Layout.t_record_to_pairs ~layout return compile_type m
    )
  | T_arrow {type1;type2} -> (
      let param' = compile_type type1 in
      let result' = compile_type type2 in
      return @@ (T_function (param',result'))
  )
  | T_module_accessor _ ->
    raise.raise @@ corner_case ~loc:__LOC__ "Module access should de resolved earlier"
  | T_singleton _ ->
    raise.raise @@ corner_case ~loc:__LOC__ "Singleton uncaught"
  | T_abstraction _ ->
    raise.raise @@ corner_case ~loc:__LOC__ "Abstraction type uncaught"
  | T_for_all _ ->
    raise.raise @@ corner_case ~loc:__LOC__ "For all type uncaught"

(* probably should use result monad for conformity? but these errors
   are supposed to be impossible *)
let internal_error loc msg =
  failwith
    (Format.asprintf
       "@[<v>Internal error, please report this as a bug@ %s@ %s@ @]"
       loc msg)

let rec build_record_accessor ~raise record path =
  match path with
  | [] -> record
  | label :: path ->
    let t =
      trace_option ~raise (corner_case ~loc:__LOC__ "Could not get record field type")
      @@ get_record_field_type (get_type_expression record) label in
    build_record_accessor ~raise
      (Ast_typed.Combinators.make_e
         (E_record_accessor { record = record ; path = label })
         t)
      path

(* todo: refactor handling of recursive functions *)
let compile_record_matching ~raise expr' return k ({ fields; body; tv } : Ast_typed.matching_content_record) =
  let record = 
    trace_option ~raise (corner_case ~loc:__LOC__ "getting lr tree") @@
    get_t_record tv in
  match record.layout with
  (* TODO unify with or simplify other case below? *)
  | L_comb ->
    let record_fields = Ast_typed.Helpers.kv_list_of_t_record_or_tuple ~layout:L_comb record.content in
    let fields =
      List.map
        ~f:(fun (l, (row_element : _ row_element_mini_c)) ->
          let t = compile_type ~raise row_element.associated_type in
          let x = trace_option ~raise
            (corner_case ~loc:__LOC__ ("missing label in record"))
            (LMap.find_opt l fields)
          in
          (fst x, t)
        )
        record_fields
    in
    let body = k body in
    return (E_let_tuple (expr', (fields, body)))
  | _ ->
    let tree = Layout.record_tree ~layout:record.layout (compile_type ~raise) record.content in
    let body = k body in
    let rec aux expr (tree : Layout.record_tree) body =
      match tree.content with
      | Field l ->
        let x = trace_option ~raise
          (corner_case ~loc:__LOC__ ("missing label in record"))
          (LMap.find_opt l fields)
        in
        let var = fst x in
        return @@ E_let_in (expr, false, ((var, tree.type_), body))
      | Pair (x, y) ->
        let x_var = Location.wrap (Var.fresh ()) in
        let y_var = Location.wrap (Var.fresh ()) in
        let x_ty = x.type_ in
        let y_ty = y.type_ in
        let x_var_expr = Combinators.Expression.make_tpl (E_variable x_var, x_ty) in
        let y_var_expr = Combinators.Expression.make_tpl (E_variable y_var, y_ty) in
        let yrec = aux y_var_expr y body in
        let xrec = aux x_var_expr x yrec in
        return @@ E_let_tuple (expr, ([(x_var, x_ty); (y_var, y_ty)], xrec))
    in
    aux expr' tree body

let rec compile_literal : AST.literal -> value = fun l -> match l with
  | Literal_int n -> D_int n
  | Literal_nat n -> D_nat n
  | Literal_timestamp n -> D_timestamp n
  | Literal_mutez n -> D_mutez n
  | Literal_bytes s -> D_bytes s
  | Literal_string s -> D_string (Ligo_string.extract s)
  | Literal_address s -> D_string s
  | Literal_signature s -> D_string s
  | Literal_key s -> D_string s
  | Literal_key_hash s -> D_string s
  | Literal_chain_id s -> D_string s
  | Literal_operation op -> D_operation op
  | Literal_unit -> D_unit

and compile_expression ~raise ?(module_env = SMap.empty) (ae:AST.expression) : expression =
  let tv = compile_type ~raise ae.type_expression in
  let self ?(module_env=module_env) = compile_expression ~raise ~module_env in
  let return ?(tv = tv) expr =
    Combinators.Expression.make_tpl ~loc:ae.location (expr, tv) in
  match ae.expression_content with
  | E_type_inst _ ->
    raise.raise @@ corner_case ~loc:__LOC__ (Format.asprintf "Type instance: This program should be monomorphised")
  | E_let_in {let_binder; rhs; let_result; attr = { inline } } ->
    let rhs' = self rhs in
    let result' = self let_result in
    return (E_let_in (rhs', inline, ((Location.map Var.todo_cast let_binder, rhs'.type_expression), result')))
  | E_type_in {type_binder=_; rhs=_; let_result} ->
    let result' = self let_result in
    result'
  | E_mod_in {module_binder; rhs; let_result} ->
    let record,module_env = compile_module_as_record ~raise module_binder module_env rhs in
    let t_record = record.type_expression in
    let result' = self ~module_env let_result in
    return @@ E_let_in (record,false, ((Location.wrap @@ Var.of_name module_binder, t_record),result'))
  | E_mod_alias {alias; binders; result} -> (
    let module_binder,access = binders in
    let module_ =
      trace_option ~raise (corner_case ~loc:__LOC__ "Mod_alias: This program shouldn't type")
      @@ SMap.find_opt  module_binder module_env  in
    let module_var = e_a_variable (Location.wrap (Var.of_name module_binder)) module_ in
    let module_expr = build_record_accessor ~raise module_var (List.map ~f:(fun l -> Label l) access) in
    let module_ = get_type_expression module_expr in
    let module_expr = self ~module_env module_expr in
    let module_env  = SMap.add alias module_ module_env in
    let result' = self ~module_env result in
    return @@ E_let_in (module_expr,false,((Location.wrap @@ Var.of_name alias, tv),result'))
  )
  | E_literal l -> return @@ E_literal l
  | E_variable name -> (
      return @@ E_variable (Location.map Var.todo_cast name)
    )
  | E_application {lamb; args} ->
      let a = self lamb in
      let b = self args in
      return @@ E_application (a, b)
  | E_constructor {constructor=Label name;element} when String.equal name "True" && element.expression_content = AST.e_unit () ->
    return @@ E_constant { cons_name = C_TRUE ; arguments = [] }
  | E_constructor {constructor=Label name;element} when String.equal name "False" && element.expression_content = AST.e_unit () ->
    return @@ E_constant { cons_name = C_FALSE ; arguments = [] }
  | E_constructor {constructor;element} -> (
    let ty' = compile_type ~raise ae.type_expression in
    let ty_variant =
      trace_option ~raise (corner_case ~loc:__LOC__ "not a record") @@
      get_t_sum (get_type_expression ae) in
    let path = Layout.constructor_to_lr ~raise ~layout:ty_variant.layout ty' ty_variant.content constructor in
    let aux = fun pred (ty, lr) ->
      let c = match lr with
        | `Left  -> C_LEFT
        | `Right -> C_RIGHT
      in
      return ~tv:ty @@ E_constant {cons_name=c;arguments=[pred]}
    in
    let element' = self element in
    let expr = List.fold ~f:aux ~init:element' path in
    expr
  )
  | E_record m -> (
      let record_t = trace_option ~raise (corner_case ~loc:__LOC__ "record expected") (AST.get_t_record ae.type_expression) in
      (* Note: now returns E_tuple, not pairs, for combs *)
      Layout.record_to_pairs ~raise self return record_t m
    )
  | E_record_accessor {record; path} -> (
    let ty' = compile_type ~raise (get_type_expression record) in
    let record_ty = trace_option ~raise (corner_case ~loc:__LOC__ "not a record") @@
      get_t_record (get_type_expression record) in
    match record_ty.layout with
    | L_comb ->
      let record_fields = Ast_typed.Helpers.kv_list_of_t_record_or_tuple ~layout:record_ty.layout record_ty.content in
      let i = fst @@ Option.value_exn  (List.findi ~f:(fun _ (label, _) -> 0 = Ast_typed.Compare.label path label) record_fields) in
      let n = List.length record_fields in
      let record = compile_expression ~raise record in
      return (E_proj (record, i, n))
    | _ ->
    let path = Layout.record_access_to_lr ~raise ~layout:record_ty.layout ty' record_ty.content path in
    let aux = fun pred (ty, lr) ->
      let c = match lr with
        | `Left  -> C_CAR
        | `Right -> C_CDR
      in
      return ~tv:ty @@ E_constant {cons_name=c;arguments=[pred]}
    in
    let record' = compile_expression ~raise record in
    let expr = List.fold ~f:aux ~init:record' path in
    expr
  )
  | E_record_update {record; path; update} -> (
    (* Compile record update to simple constructors &
       projections. This will be optimized to some degree by eta
       contraction in a later pass. *)
      let ty = get_type_expression record in
      let record_ty =
        trace_option ~raise (corner_case ~loc:__LOC__ "not a record") @@
        get_t_record (ty) in
      let ty' = compile_type ~raise (ty) in
      match record_ty.layout with
      | L_comb ->
        let record_fields = Ast_typed.Helpers.kv_list_of_t_record_or_tuple ~layout:record_ty.layout record_ty.content in
        let record = self record in
        let update = self update in
        let i = fst @@ Option.value_exn  (List.findi ~f:(fun _ (label, _) -> 0 = Ast_typed.Compare.label path label) record_fields) in
        let n = List.length record_fields in
        return (E_update (record, i, update, n))
      | _ ->
      let path =
        trace_strong ~raise (corner_case ~loc:__LOC__ "record access") @@
        (fun ~raise:_ -> Layout.record_access_to_lr ~raise ~layout:record_ty.layout ty' record_ty.content path) in
      let path = List.map ~f:snd path in
      let update = self update in
      let record = self record in
      let record_var = Var.fresh () in
      let car (e : expression) : expression =
        match e.type_expression.type_content with
        | T_tuple [(_, a); _] ->
          { e with
            content = E_constant { cons_name = C_CAR ; arguments = [e] } ;
            type_expression = a }
        | _ -> internal_error __LOC__ "record did not have pair type" in
      let cdr (e : expression) : expression =
        match e.type_expression.type_content with
        | T_tuple [_; (_, b)] ->
          { e with
            content = E_constant { cons_name = C_CDR ; arguments = [e] } ;
            type_expression = b }
        | _ -> internal_error __LOC__ "record did not have pair type" in
      let rec build_record_update record path =
        match path with
        | [] -> update
        | `Left :: path ->
          { record with
            content = E_constant { cons_name = C_PAIR ;
                                   arguments = [ build_record_update (car record) path;
                                                 cdr record ] } }
        | `Right :: path ->
          { record with
            content = E_constant { cons_name = C_PAIR ;
                                   arguments = [ car record;
                                                 build_record_update (cdr record) path ] } } in
      return
        (E_let_in (record, false, ((Location.wrap record_var, record.type_expression),
                   build_record_update
                     (e_var (Location.wrap record_var) record.type_expression)
                     path)))
  )
  | E_constant {cons_name=name; arguments=lst} -> (
      let iterator_generator iterator_name =
        let expression_to_iterator_body (f : AST.expression) =
          let (input , output) = trace_option ~raise (corner_case ~loc:__LOC__ "expected function type") @@ AST.get_t_function f.type_expression in
          let f' = self f in
          let input' = compile_type ~raise input in
          let output' = compile_type ~raise output in
          let binder = Location.wrap @@ Var.fresh ~name:"iterated" () in
          let application = Mini_c.Combinators.e_application f' output' (Mini_c.Combinators.e_var binder input') in
          ((binder , input'), application)
        in
        fun (lst : AST.expression list) -> match (lst , iterator_name) with
          | [f ; i] , C_ITER | [f ; i] , C_MAP -> (
              let f' = expression_to_iterator_body f in
              let i' = self i in
              return @@ E_iterator (iterator_name , f' , i')
            )
          | [ f ; collection ; initial ] , C_FOLD -> (
              let f' = expression_to_iterator_body f in
              let initial' = self initial in
              let collection' = self collection in
              return @@ E_fold (f' , collection' , initial')
            )
          | [ f ; initial ; collection ], C_FOLD_LEFT -> (
              let f' = expression_to_iterator_body f in
              let initial' = self initial in
              let collection' = self collection in
              return @@ E_fold (f' , collection' , initial')
            )
          | [ f ; collection ; initial ], C_FOLD_RIGHT -> (
              let f' = expression_to_iterator_body f in
              let initial' = self initial in
              let elem_type = 
                (trace_option ~raise (corner_case ~loc:__LOC__ "Wrong type : expecting collection")) @@
                get_t_collection @@ compile_type ~raise collection.type_expression in
              let collection' = self collection in
              return @@ E_fold_right (f' , (collection',elem_type) , initial')
            )
          | _ -> raise.raise @@ corner_case ~loc:__LOC__ (Format.asprintf "bad iterator arity: %a" PP.constant iterator_name)
      in
      let (iter , map , fold, fold_left, fold_right) = iterator_generator C_ITER,
                                                       iterator_generator C_MAP,
                                                       iterator_generator C_FOLD,
                                                       iterator_generator C_FOLD_LEFT,
                                                       iterator_generator C_FOLD_RIGHT in
      match (name , lst) with
      | (C_SET_ITER , lst) -> iter lst
      | (C_LIST_ITER , lst) -> iter lst
      | (C_MAP_ITER , lst) -> iter lst
      | (C_LIST_MAP , lst) -> map lst
      | (C_MAP_MAP , lst) -> map lst
      | (C_LIST_FOLD , lst) -> fold lst
      | (C_SET_FOLD , lst) -> fold lst
      | (C_MAP_FOLD , lst) -> fold lst
      | (C_FOLD, lst) -> fold lst
      | (C_LIST_FOLD_LEFT, lst) -> fold_left lst
      | (C_LIST_FOLD_RIGHT, lst) -> fold_right lst
      | (C_SET_FOLD_DESC , lst) -> fold_right lst
      | _ -> (
          let lst' = List.map ~f:(self) lst in
          return @@ E_constant {cons_name=compile_constant' name;arguments=lst'}
        )
    )
  | E_lambda l ->
    let io = trace_option ~raise (corner_case ~loc:__LOC__ "expected function type") @@
      AST.get_t_function ae.type_expression in
    compile_lambda ~raise module_env l io
  | E_recursive r ->
    compile_recursive ~raise module_env r
  | E_matching {matchee=expr; cases=m} -> (
      let expr' = self expr in
      match m with
      | Match_variant {cases ; tv} -> (
          match expr.type_expression.type_content with
          | T_constant { injection ; parameters = [list_ty] } when String.equal (Ligo_string.extract injection) Stage_common.Constant.list_name ->
            let list_ty = compile_type ~raise list_ty in
            let get_c_body (case : AST.matching_content_case) = (case.constructor, (case.body, case.pattern)) in
            let c_body_lst = AST.LMap.of_list (List.map ~f:get_c_body cases) in
            let get_case c =
              trace_option ~raise
                (corner_case ~loc:__LOC__ ("missing " ^ c ^ " case in match"))
                (AST.LMap.find_opt (Label c) c_body_lst) in
            let match_nil = get_case "Nil" in
            let match_cons = get_case "Cons" in
            let nil = self (fst match_nil) in
            let cons =
              let hd = Location.wrap @@ Var.fresh () in
              let tl = Location.wrap @@ Var.fresh () in
              let proj_t = t_pair (None,list_ty) (None,expr'.type_expression) in
              let proj = Expression.make (ec_pair (e_var hd list_ty) (e_var tl expr'.type_expression)) proj_t in
              let cons_body = self (fst match_cons) in
              let cons_body' = e_let_in (snd match_cons) proj_t false proj cons_body in
              (((hd,list_ty), (tl,expr'.type_expression)), cons_body')
            in
            return @@ E_if_cons (expr' , nil , cons)
          | T_constant { injection ; parameters = [opt_tv] } when String.equal (Ligo_string.extract injection) Stage_common.Constant.option_name ->
            let get_c_body (case : AST.matching_content_case) = (case.constructor, (case.body, case.pattern)) in
            let c_body_lst = AST.LMap.of_list (List.map ~f:get_c_body cases) in
            let get_case c =
              trace_option ~raise
                (corner_case ~loc:__LOC__ ("missing " ^ c ^ " case in match"))
                (AST.LMap.find_opt (Label c) c_body_lst) in
            let match_none = get_case "None" in
            let match_some = get_case "Some" in
            let n = self (fst match_none) in
            let (tv' , s') =
              let tv' = compile_type ~raise opt_tv in
              let s' = self (fst match_some) in
              (tv' , s')
            in
            return @@ E_if_none (expr' , n , ((Location.map Var.todo_cast (snd match_some) , tv') , s'))
          | T_sum _ when Option.is_some (Ast_typed.get_t_bool expr.type_expression) ->
            let ctor_body (case : AST.matching_content_case) = (case.constructor, case.body) in
            let cases = AST.LMap.of_list (List.map ~f:ctor_body cases) in
            let get_case c =
              trace_option ~raise
                (corner_case ~loc:__LOC__ ("missing " ^ c ^ " case in match"))
                (AST.LMap.find_opt (Label c) cases) in
            let match_true  = get_case "True" in
            let match_false = get_case "False" in
            let (t , f) = Pair.map ~f:self (match_true, match_false) in
            return @@ E_if_bool (expr', t, f)
          | _ -> (
              let record_ty = trace_option ~raise (corner_case ~loc:__LOC__ "getting lr tree") @@
                get_t_sum tv in
              let tree = Layout.match_variant_to_tree ~raise ~layout:record_ty.layout ~compile_type:(compile_type ~raise) record_ty.content in
              let rec aux top t =
                match t with
                | ((`Leaf (Label constructor_name)) , tv) -> (
                    let ({constructor=_ ; pattern ; body} : AST.matching_content_case ) =
                      trace_option ~raise (corner_case ~loc:__LOC__ "missing match clause") @@
                      let aux ({constructor = Label c ; pattern=_ ; body=_} : AST.matching_content_case) =
                        (c = constructor_name) in
                      List.find ~f:aux cases in
                    let body' = self body in
                    return @@ E_let_in (top, false, ((Location.map Var.todo_cast pattern , tv) , body'))
                  )
                | ((`Node (a , b)) , tv) ->
                  let a' =
                    let a_ty = trace_option ~raise (corner_case ~loc:__LOC__ "wrongtype") @@ get_t_left tv in
                    let left_var = Location.wrap @@ Var.fresh ~name:"left" () in
                    let e = aux (((Expression.make (E_variable left_var) a_ty))) a in
                    ((left_var , a_ty) , e)
                  in
                  let b' =
                    let b_ty = trace_option ~raise (corner_case ~loc:__LOC__ "wrongtype") @@ get_t_right tv in
                    let right_var = Location.wrap @@ Var.fresh ~name:"right" () in
                    let e = aux (((Expression.make (E_variable right_var) b_ty))) b in
                    ((right_var , b_ty) , e)
                  in
                  return @@ E_if_left (top , a' , b')
              in
              trace_strong ~raise (corner_case ~loc:__LOC__ "building constructor") @@
              (fun ~raise:_ -> aux expr' tree)
            )
      )
      | Match_record record ->
        compile_record_matching ~raise expr' return self record
  )
  | E_raw_code { language; code} ->
    let backend = Stage_common.Backends.michelson in
    let () =
      Assert.assert_true ~raise
        (corner_case ~loc:__LOC__ "Language insert - backend mismatch only provide code insertion in the language you are compiling to")
        (String.equal language backend)
    in
    let type_anno  = get_type_expression code in
    let type_anno' = compile_type ~raise type_anno in
    let code = trace_option ~raise (corner_case ~loc:__LOC__ "could not get a string") @@ get_a_string code in
    let open Tezos_micheline in
    let orig_code = code in
    let (code, errs) = Micheline_parser.tokenize code in
    (match errs with
    | _ :: _ -> raise.raise (could_not_parse_raw_michelson ae.location orig_code)
    | [] ->
      let (code, errs) = Micheline_parser.parse_expression ~check:false code in
      match errs with
      | _ :: _ -> raise.raise (could_not_parse_raw_michelson ae.location orig_code)
      | [] ->
        let code = Micheline.strip_locations code in
        (* hmm *)
        let code = Micheline.inject_locations (fun _ -> Location.generated) code in
        match code with
        | Seq (_, code) ->
          return ~tv:type_anno' @@ E_raw_michelson code
        | _ ->
          raise.raise (raw_michelson_must_be_seq ae.location code)
    )
  | E_module_accessor {module_name; element} -> (
    let module_var = module_name in
    let module_ =
      trace_option ~raise (corner_case ~loc:__LOC__ "Module_accessor: This program shouldn't type")
      @@ SMap.find_opt module_var module_env in
    (* TODO E_module_accessor should not be this way *)
    let rec aux (element : AST.expression) =
      match element.expression_content with
      | E_module_accessor {module_name; element} ->
        let module_var = module_name in
        let module_names = aux element in
        (module_var :: module_names)
      | E_variable var ->
        [Format.asprintf "%a" Var.pp @@ Location.unwrap var]
      | E_record_accessor {record; path} ->
        let module_names = aux record in
        let Label module_var = path in
        (module_names @ [module_var])
      | _ -> raise.raise @@ corner_case ~loc:__LOC__ "The parser shouldn't allowed this construct"
    in
    let access_list = aux element in
    let module_var = e_a_variable (Location.wrap (Var.of_name module_var)) module_ in
    let expr = build_record_accessor ~raise module_var (List.map ~f:(fun l -> Label l) access_list) in
    let expr = self ~module_env expr in
    expr
  )

and compile_lambda ~raise module_env l (input_type , output_type) =
  let { binder ; result } : AST.lambda = l in
  let result' = compile_expression ~raise ~module_env result in
  let input = compile_type ~raise input_type in
  let output = compile_type ~raise output_type in
  let tv = Combinators.t_function input output in
  let binder = Location.map Var.todo_cast binder in
  let closure = E_closure { binder; body = result'} in
  Combinators.Expression.make_tpl ~loc:result.location (closure , tv)

and compile_recursive ~raise module_env {fun_name; fun_type; lambda} =
  let rec map_lambda : AST.expression_variable -> type_expression -> AST.expression -> expression * expression_variable list = fun fun_name loop_type e ->
    match e.expression_content with
      E_lambda {binder;result} ->
      let binder = Location.map Var.todo_cast binder in
      let (body,l) = map_lambda  fun_name loop_type result in
      (Expression.make ~loc:e.location (E_closure {binder;body}) loop_type, binder::l)
    | _  ->
      let res = replace_callback ~raise fun_name loop_type false e in
      (res, [])

  and replace_callback ~raise : AST.expression_variable -> type_expression -> bool -> AST.expression -> expression = fun fun_name loop_type shadowed e ->
    match e.expression_content with
      | E_let_in li ->
        let shadowed = shadowed || Var.equal li.let_binder.wrap_content fun_name.wrap_content in
        let let_result = replace_callback ~raise fun_name loop_type shadowed li.let_result in
        let rhs = compile_expression ~raise ~module_env li.rhs in
        let ty  = compile_type ~raise li.rhs.type_expression in
        e_let_in (Location.map Var.todo_cast li.let_binder) ty li.attr.inline rhs let_result
      | E_matching m ->
        let ty = compile_type ~raise e.type_expression in
        matching ~raise fun_name loop_type shadowed m ty
      | E_application {lamb;args} -> (
        match lamb.expression_content,shadowed with
        | E_variable name, false when Var.equal fun_name.wrap_content name.wrap_content ->
          let expr = compile_expression ~raise ~module_env args in
          Expression.make (E_constant {cons_name=C_LOOP_CONTINUE;arguments=[expr]}) loop_type
        | _ ->
          let expr = compile_expression ~raise ~module_env e in
          Expression.make (E_constant {cons_name=C_LOOP_STOP;arguments=[expr]}) loop_type
      )
      | _ ->
        let expr = compile_expression ~raise ~module_env e in
        Expression.make (E_constant {cons_name=C_LOOP_STOP;arguments=[expr]}) loop_type

  and matching ~raise : AST.expression_variable -> type_expression -> bool -> AST.matching -> type_expression -> expression = fun fun_name loop_type shadowed m ty ->
    let return ret = Expression.make ret @@ ty in
    let expr' = compile_expression ~raise ~module_env m.matchee in
    let self = replace_callback ~raise fun_name loop_type shadowed in
    match m.cases with
    | Match_variant {cases ; tv} -> (
        match m.matchee.type_expression.type_content with
        | T_constant { injection ; parameters = [list_ty] } when String.equal (Ligo_string.extract injection) Stage_common.Constant.list_name ->
          let list_ty = compile_type ~raise list_ty in
          let get_c_body (case : AST.matching_content_case) = (case.constructor, (case.body, case.pattern)) in
          let c_body_lst = AST.LMap.of_list (List.map ~f:get_c_body cases) in
          let get_case c =
            trace_option ~raise
              (corner_case ~loc:__LOC__ ("missing " ^ c ^ " case in match"))
              (AST.LMap.find_opt (Label c) c_body_lst) in
          let match_nil = get_case "Nil" in
          let match_cons = get_case "Cons" in
          let nil = self (fst match_nil) in
          let cons =
            let hd = Location.wrap @@ Var.fresh () in
            let tl = Location.wrap @@ Var.fresh () in
            let proj_t = t_pair (None,list_ty) (None,expr'.type_expression) in
            let proj = Expression.make (ec_pair (e_var hd list_ty) (e_var tl expr'.type_expression)) proj_t in
            let cons_body = self (fst match_cons) in
            let cons_body' = e_let_in (snd match_cons) proj_t false proj cons_body in
            (((hd,list_ty), (tl,expr'.type_expression)), cons_body')
          in
          return @@ E_if_cons (expr' , nil , cons)
        | T_constant { injection ; parameters = [opt_tv] } when String.equal (Ligo_string.extract injection) Stage_common.Constant.option_name ->
          let get_c_body (case : AST.matching_content_case) = (case.constructor, (case.body, case.pattern)) in
          let c_body_lst = AST.LMap.of_list (List.map ~f:get_c_body cases) in
          let get_case c =
            trace_option ~raise
              (corner_case ~loc:__LOC__ ("missing " ^ c ^ " case in match"))
              (AST.LMap.find_opt (Label c) c_body_lst) in
          let match_none = get_case "None" in
          let match_some = get_case "Some" in
          let n = self (fst match_none) in
          let (tv' , s') =
            let tv' = compile_type ~raise opt_tv in
            let s' = self (fst match_some) in
            (tv' , s')
          in
          return @@ E_if_none (expr' , n , ((Location.map Var.todo_cast (snd match_some) , tv') , s'))
        | T_sum _ when Option.is_some (Ast_typed.get_t_bool m.matchee.type_expression) ->
          let ctor_body (case : AST.matching_content_case) = (case.constructor, case.body) in
          let cases = AST.LMap.of_list (List.map ~f:ctor_body cases) in
          let get_case c =
            trace_option ~raise
              (corner_case ~loc:__LOC__ ("missing " ^ c ^ " case in match"))
              (AST.LMap.find_opt (Label c) cases) in
          let match_true  = get_case "True" in
          let match_false = get_case "False" in
          let (t , f) = Pair.map ~f:self (match_true, match_false) in
          return @@ E_if_bool (expr', t, f)
        | _ -> (
            let record_ty = trace_option ~raise (corner_case ~loc:__LOC__ "getting lr tree") @@
              get_t_sum tv in
            let tree = Layout.match_variant_to_tree ~raise ~layout:record_ty.layout ~compile_type:(compile_type ~raise) record_ty.content in
            let rec aux top t =
              match t with
              | ((`Leaf (Label constructor_name)) , tv) -> (
                  let ({constructor=_ ; pattern ; body} : AST.matching_content_case)=
                    trace_option ~raise (corner_case ~loc:__LOC__ "missing match clause") @@
                    let aux ({constructor = Label c ; pattern=_ ; body=_} : AST.matching_content_case) =
                      (c = constructor_name) in
                    List.find ~f:aux cases in
                  let body' = self body in
                  return @@ E_let_in (top, false, ((Location.map Var.todo_cast pattern , tv) , body'))
                )
              | ((`Node (a , b)) , tv) ->
                let a' =
                  let a_ty = trace_option ~raise (corner_case ~loc:__LOC__ "wrongtype") @@ get_t_left tv in
                  let left_var = Location.wrap @@ Var.fresh ~name:"left" () in
                  let e = aux (((Expression.make (E_variable left_var) a_ty))) a in
                  ((left_var , a_ty) , e)
                in
                let b' =
                  let b_ty = trace_option ~raise (corner_case ~loc:__LOC__ "wrongtype") @@ get_t_right tv in
                  let right_var = Location.wrap @@ Var.fresh ~name:"right" () in
                  let e = aux (((Expression.make (E_variable right_var) b_ty))) b in
                  ((right_var , b_ty) , e)
                in
                return @@ E_if_left (top , a' , b')
          in
          trace_strong ~raise (corner_case ~loc:__LOC__ "building constructor") @@
          (fun ~raise:_ -> aux expr' tree)
       )
      )
      | Match_record record ->
        compile_record_matching ~raise expr' return (replace_callback ~raise fun_name loop_type shadowed) record
  in
  let fun_type = compile_type ~raise fun_type in
  let (input_type,output_type) = trace_option ~raise (corner_case ~loc:__LOC__ "wrongtype") @@ get_t_function fun_type in
  let loop_type = t_union (None, input_type) (None, output_type) in
  let (body,binder) = map_lambda fun_name loop_type lambda.result in
  let binder = Location.map Var.todo_cast lambda.binder :: binder in
  let binder = match binder with hd::[] -> hd | _ -> raise.raise @@ unsupported_recursive_function fun_name in
  let expr = Expression.make_tpl (E_variable binder, input_type) in
  let body = Expression.make (E_iterator (C_LOOP_LEFT, ((lambda.binder, input_type), body), expr)) output_type in
  Expression.make (E_closure {binder;body}) fun_type

and compile_declaration ~raise module_env env (d:AST.declaration) : (toplevel_statement * _ SMap.t) option =
  match d with
  | Declaration_type _ -> None
  | Declaration_constant { binder ; expr ; attr = { inline } } ->
    let expression = compile_expression ~raise ~module_env expr in
    let binder = Location.map Var.todo_cast binder in
    let tv = Combinators.Expression.get_type expression in
    let env' = Environment.add (binder, tv) env in
    Some (((binder, inline, expression), environment_wrap env env'), module_env)
  | Declaration_module {module_binder; module_} ->
    let record,module_env = compile_module_as_record ~raise module_binder module_env module_ in
    let binder = Location.wrap @@ Var.of_name module_binder in
    let tv = Combinators.Expression.get_type record in
    let env' = Environment.add (binder, tv) env in
    Some (((binder, false, record), environment_wrap env env'), module_env)
  | Module_alias {alias; binders} ->
    let module_var, access = binders in
    let module_ =
      trace_option ~raise (corner_case ~loc:__LOC__ "Mod_alias: This program shouldn't type")
      @@ SMap.find_opt module_var module_env  in
    let module_var = e_a_variable (Location.wrap (Var.of_name module_var)) module_ in
    let module_expr = build_record_accessor ~raise module_var (List.map ~f:(fun l -> Label l) access) in
    let module_ = get_type_expression module_expr in
    let module_expr = compile_expression ~raise ~module_env module_expr in
    let module_env  = SMap.add alias module_ module_env in
    let module_type = compile_type ~raise module_ in
    let env' = Environment.add (Location.wrap @@ Var.of_name alias, module_type) env in
    Some ((((Location.wrap @@ Var.of_name alias),true,module_expr),environment_wrap env env'),module_env)



and compile_module ~raise ?(module_env = SMap.empty) ((AST.Module_Fully_Typed lst) : AST.module_fully_typed) : program * AST.type_expression SMap.t =
  let aux (prev:toplevel_statement list * _ SMap.t * Environment.t) cur =
    let (hds, module_env, env) = prev in
    let x = compile_declaration ~raise module_env env cur in
    match x with
    | Some (((_ , env') as cur', module_env)) -> (hds @ [ cur' ] , module_env, env'.post_environment)
    | None -> prev
  in
  let (statements,map, _) = List.fold_left ~f:aux ~init:([], module_env,Environment.empty) (temp_unwrap_loc_list lst) in
  (statements, map)

and compile_module_as_record ~raise module_name (module_env : _ SMap.t) (lst : AST.module_fully_typed) : (expression * _)=
  let rec module_as_record ~raise module_env (AST.Module_Fully_Typed lst) : (AST.expression * _) =
    let aux (r,env) (cur : AST.declaration ) =
      match cur with
      | Declaration_constant { binder ; expr; attr=_ } ->
        let l = Format.asprintf "%a" Var.pp @@ Location.unwrap binder in
        let attr : AST.known_attributes = { inline = false ; no_mutation = false; public = true; view = false } in
        ((Label l,(expr,attr))::r,env)
      | Declaration_type _ty -> (r,env)
      | Declaration_module {module_binder; module_} ->
        let l = module_binder in
        let r',_ = module_as_record env ~raise module_ in
        let env = SMap.add l (get_type_expression r') env in
        let attr : AST.known_attributes = { inline = false ; no_mutation = false; public = true ; view = false } in
        ((Label l,(r',attr))::r,env)
      | Module_alias {alias; binders} ->
        let l = alias in
        let module_var, access = binders in
        let module_type =
          trace_option ~raise (corner_case ~loc:__LOC__ "Mod_alias: This program shouldn't type")
          @@ SMap.find_opt module_var env in
        let module_var = e_a_variable (Location.wrap (Var.of_name module_var)) module_type in
        let module_expr = build_record_accessor ~raise module_var (List.map ~f:(fun l -> Label l) access) in
        let module_type = get_type_expression module_expr in
        let env = SMap.add l module_type env in
        let r' = module_expr in
        let attr : AST.known_attributes = { inline = true ; no_mutation = false ; public = true ; view = false } in
        ((Label l,(r',attr))::r,env)

    in
    let r,env = List.fold ~f:aux ~init:([],module_env) (temp_unwrap_loc_list lst) in
    if List.length r <> 0 then begin
      let build_record r =
        let aux (Label l, ((expr : AST.expression), _)) =
          let expr = {expr with expression_content = e_variable @@ Location.wrap @@ Var.of_name l} in
          (Label l, expr)
        in
        let record = ez_e_a_record @@ List.map ~f:aux (List.rev r) in
      (* prefix with let_in*)
        let aux record (Label l, (expr, inline)) =
          let binder = Location.wrap @@ Var.of_name l in
          AST.e_a_let_in binder expr record inline
        in
        let record = List.fold_left ~f:aux ~init:record r in
        record
      in
      let record = build_record r in
      (record,env)
    end
    else
      (e_a_unit,env)
  in
  let record,env = module_as_record ~raise module_env lst in
  let module_env = SMap.add module_name (get_type_expression record) env in
  let record = compile_expression ~raise ~module_env record in
  (record, module_env)
