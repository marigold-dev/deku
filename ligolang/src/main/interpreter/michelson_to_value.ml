open Trace
open Ligo_interpreter.Types
open Tezos_micheline.Micheline

let contract_of_string ~raise s =
  Proto_alpha_utils.Trace.trace_alpha_tzresult ~raise (fun _ -> Errors.generic_error Location.generated "Cannot parse address") @@ Tezos_protocol_011_PtHangzH.Protocol.Alpha_context.Contract.of_b58check s
let key_hash_of_string ~raise s =
  Proto_alpha_utils.Trace.trace_tzresult ~raise (fun _ -> Errors.generic_error Location.generated "Cannot parse address") @@ Tezos_crypto.Signature.Public_key_hash.of_b58check s

let wrong_mini_c_value _t _v = Errors.generic_error Location.generated "wrong_mini_c_value"
let corner_case ~loc s = ignore loc; Errors.generic_error Location.generated @@ "corner_case: " ^ s
let bad_decompile _ = Errors.generic_error Location.generated "bad_decompile"
let untranspilable t v =
  let v = v |> Tezos_micheline.Micheline.map_node (fun _ -> {Tezos_micheline.Micheline_printer.comment = None}) (fun x -> x) in
  let t = t |> Tezos_micheline.Micheline.map_node (fun _ -> {Tezos_micheline.Micheline_printer.comment = None}) (fun x -> x) in
  let s = Format.asprintf " %a %a" Tezos_micheline.Micheline_printer.print_expr t Tezos_micheline.Micheline_printer.print_expr v in
  Errors.generic_error Location.generated ("untranspilable" ^ s)

let rec comb prim loc xs =
  match xs with
  | [] | [_] -> assert false
  | [x1; x2] -> Prim (loc, prim, [x1; x2], [])
  | x1 :: x2 :: xs ->
    let xs = comb prim loc (x2 :: xs) in
    Prim (loc, prim, [x1; xs], [])

let normalize_edo_comb_type =
  function
  | Prim (loc, "pair", xs, _) ->
    comb "pair" loc xs
  | t -> t

let normalize_edo_comb_value =
  function
  (* only do it for type is "pair", because Seq case is ambiguous *)
  | Prim (_, "pair", _, _) ->
    (function
      | Prim (loc, "Pair", xs, _) ->
        comb "Pair" loc xs
      | Seq (loc, xs) ->
        comb "Pair" loc xs
      | x -> x)
  | _ -> fun x -> x

let rec decompile_to_untyped_value ~raise ~bigmaps :
   ('l, string) node -> ('l, string) node -> Ligo_interpreter.Types.value =
  fun ty value ->
  let ty = normalize_edo_comb_type ty in
  let value = normalize_edo_comb_value ty value in
  match (ty, value) with
  | Prim (_, "pair", ts, _), Prim (_, "Pair", vs, _) -> (
      let els = List.map ~f:(fun (t,v) -> decompile_to_untyped_value ~raise ~bigmaps t v) (List.zip_exn ts vs) in
      let rec aux l : value =
        match l with
        | [] -> raise.raise (untranspilable ty value)
        | [x] -> x
        | hd::tl -> (
            let tl' = aux tl in
            Ligo_interpreter.Combinators.v_pair (hd, tl')
          ) in
      aux els
    )
  | Prim (_, "or", [a_ty; _], _), Prim (_, "Left", [a], _) -> (
      let a = decompile_to_untyped_value ~raise ~bigmaps a_ty a in
      V_Construct ("Left", a)
    )
  | Prim (_, "or", [_; b_ty], _), Prim (_, "Right", [b], _) -> (
      let b = decompile_to_untyped_value ~raise ~bigmaps b_ty b in
      V_Construct ("Right", b)
    )
  | Prim (_, "int", [], _), Int (_, n) ->
      V_Ct (C_int n)
  | Prim (_, "nat", [], _), Int (_, n) ->
      V_Ct (C_nat n)
  (* | Prim (_, "chain_id", _, _), String (_, id) ->
   *   (\* Before EDO :
   *     let id = Tezos_base.TzPervasives.Chain_id.of_bytes_exn id in
   *     let str = Tezos_crypto.Base58.simple_encode
   *     (Tezos_base__TzPervasives.Chain_id.b58check_encoding)
   *     id in
   *   *\)
   *   D_string id *)
  | Prim (_, "key_hash", [], _), String (_, n) ->
     V_Ct (C_key_hash (key_hash_of_string ~raise n))
  (* | Prim (_, "key", [], _), String (_, n) ->
   *   D_string n *)
  (* | Prim (_, "signature", [], _), String (_, n) ->
   *   D_string n *)
  | Prim (_, "timestamp", [], _), Int (_, n) ->
      V_Ct (C_timestamp n)
  | Prim (_, "timestamp", [], _), String (_, n) ->
      let open Tezos_base.TzPervasives.Time.Protocol in
      let n = Z.of_int64 (to_seconds (of_notation_exn n)) in
      V_Ct (C_timestamp n)
  | Prim (_, "mutez", [], _), Int (_, n) ->
      V_Ct (C_mutez n)
  | Prim (_, "bool", [], _), Prim (_, "True", [], _) ->
      V_Ct (C_bool true)
  | Prim (_, "bool", [], _), Prim (_, "False", [], _) ->
      V_Ct (C_bool false)
  | Prim (_, "string", [], _), String (_, s) ->
      V_Ct (C_string s)
  | Prim (_, "bytes", [], _), Bytes (_, b) ->
      V_Ct (C_bytes b)
  | Prim (_, "address", [], _), Bytes (_, b) ->
      let open Proto_alpha_utils in
      let enc = Memory_proto_alpha.Protocol.Alpha_context.Contract.encoding in
      let c = Data_encoding.Binary.of_bytes_exn enc b in
      V_Ct (C_address c)
  | Prim (_, "address", [], _), String (_, s) ->
      V_Ct (C_address (contract_of_string ~raise s))
  | Prim (_, "unit", [], _), Prim (_, "Unit", [], _) ->
      V_Ct (C_unit)
  | Prim (_, "option", [_], _), Prim (_, "None", [], _) ->
      Ligo_interpreter.Combinators.v_none ()
  | Prim (_, "option", [o_ty], _), Prim (_, "Some", [s], _) ->
      let s' = decompile_to_untyped_value ~raise ~bigmaps o_ty s in
      Ligo_interpreter.Combinators.v_some s'
  | Prim (_, "map", [k_ty; v_ty], _), Seq (_, lst) ->
      let lst' =
        let aux elt =
          match elt with
          | Prim (_, "Elt", [k; v], _) ->
            let k' = decompile_to_untyped_value ~raise ~bigmaps k_ty k in
            let v' = decompile_to_untyped_value ~raise ~bigmaps v_ty v in
            (k', v')
          | _ ->
            let ty = root (strip_locations ty) in
            let value = root (strip_locations value) in
            raise.raise (untranspilable ty value)
        in
        List.map ~f:aux lst
      in
      V_Map lst'
  | Prim (_, "big_map", [k_ty; v_ty], _), Seq (_, lst) ->
      let lst' =
        let aux elt =
          match elt with
          | Prim (_, "Elt", [k; v], _) ->
            let k' = decompile_to_untyped_value ~raise ~bigmaps k_ty k in
            let v' = decompile_to_untyped_value ~raise ~bigmaps v_ty v in
            (k', v')
          | _ ->
            let ty = root (strip_locations ty) in
            let value = root (strip_locations value) in
            raise.raise (untranspilable ty value)
        in
        List.map ~f:aux lst
      in
      V_Map lst'
  | Prim (_, "big_map", [_; _], _), Int (_, v) ->
     let data : Ligo_interpreter.Types.bigmap_data = List.Assoc.find_exn bigmaps ~equal:Int.equal (Z.to_int v) in
     let lst = data.version in
     V_Map lst
  | Prim (_, "list", [ty], _), Seq (_, lst) ->
      let lst' =
        List.map ~f:(fun v -> decompile_to_untyped_value ~raise ~bigmaps ty v) lst
      in
      V_List lst'
  | Prim (_, "set", [ty], _), Seq (_, lst) -> (
      let lst' =
        let aux acc cur = cur :: acc in
        let lst = List.fold_left ~f:aux ~init:lst [] in
        List.rev lst in
      let lst'' =
        let aux = fun v -> decompile_to_untyped_value ~raise ~bigmaps ty v in
        List.map ~f:aux lst'
      in
      V_Set lst''
    )
  (* | Prim (_, "operation", [], _), Bytes (_, op) -> (
   *     D_operation op
   *   ) *)
  | Prim (_, "lambda", [_; _], _), ((Seq (_, _)) as c) ->
      let open! Ast_typed in
      let arg_binder = Var.fresh () in
      let arg_binder = Location.wrap arg_binder in
      (* These are temporal types, need to be patched later: *)
      let t_input = t_unit () in
      let t_output = t_unit () in
      let c = Tezos_micheline.Micheline.strip_locations c in
      let c = Proto_alpha_utils.Trace.trace_alpha_tzresult ~raise (fun _ -> Errors.generic_error Location.generated "Cannot get instructions") @@ Tezos_protocol_011_PtHangzH.Protocol.Michelson_v1_primitives.prims_of_strings c in
      let u = Format.asprintf "%a" Tezos_micheline.Micheline_printer.print_expr
            (Tezos_micheline.Micheline_printer.printable Tezos_protocol_011_PtHangzH.Protocol.Michelson_v1_primitives.string_of_prim c)
      in
      let code_block = make_e (e_string (Ligo_string.verbatim u)) (t_string ()) in
      let insertion = e_a_raw_code Stage_common.Backends.michelson code_block (t_function t_input t_output ()) in
      let body = e_a_application insertion (e_a_variable arg_binder t_input) t_output in
      let orig_lambda = e_a_lambda {binder=arg_binder; result=body} t_input t_output in
      V_Func_val {rec_name = None; orig_lambda; arg_binder; body; env = Ligo_interpreter.Environment.empty_env }
  (* | Prim (xx, "ticket", [ty], _) , Prim (_, "Pair", [addr;v;amt], _) ->
   *   ignore addr;
   *   let ty_nat = Prim (xx, "nat", [], []) in
   *   let v' = decompile_to_mini_c ~raise ~bigmaps ty v in
   *   let amt' = decompile_to_mini_c ~raise ~bigmaps ty_nat amt in
   *   D_ticket (v', amt') *)
  | ty, v ->
    raise.raise (untranspilable ty v)

let rec decompile_value ~raise ~(bigmaps : bigmap list) (v : value) (t : Ast_typed.type_expression) : value =
  let open Stage_common.Constant in
  let open Ligo_interpreter.Combinators in
  let open! Ast_typed in
  let self = decompile_value ~raise ~bigmaps in
  match t.type_content with
  | tc when (Ast_typed.Compare.type_content tc (t_bool ()).type_content) = 0->
     v
  | T_constant { language; injection; parameters } -> (
    let () = Assert.assert_true ~raise
      (corner_case ~loc:__LOC__ ("unsupported language "^language))
      (String.equal language Stage_common.Backends.michelson)
    in
    match (Ligo_string.extract injection,parameters) with
    | (i, [o]) when String.equal i option_name -> (
        let opt = trace_option ~raise (wrong_mini_c_value t v) @@ get_option v in
        match opt with
        | None -> v_none ()
        | Some s ->
            let s' = self s o in
            v_some s'
      )
    | (i, [k_ty;v_ty]) when String.equal i map_name -> (
        let map = trace_option ~raise (wrong_mini_c_value t v) @@ get_map v in
        let map' =
          let aux = fun (k, v) ->
            let key   = self k k_ty in
            let value = self v v_ty in
            (key, value) in
          List.map ~f:aux map in
        V_Map map'
      )
    | (i, [k_ty; v_ty]) when String.equal i big_map_name -> (
        match get_nat v with
        | Some _ ->
           raise.raise @@ corner_case ~loc:"unspiller" "Big map id not supported"
        | None ->
        let big_map = trace_option ~raise (wrong_mini_c_value t v) @@ get_map v in
        let big_map' =
          let aux = fun (k, v) ->
            let key   = self k k_ty in
            let value = self v v_ty in
            (key, value) in
          List.map ~f:aux big_map in
        V_Map big_map'
      )
    | (i, _) when String.equal i map_or_big_map_name -> raise.raise @@ corner_case ~loc:"unspiller" "TC_map_or_big_map t should not be present in mini-c"
    | (i, [ty]) when String.equal i list_name -> (
        let lst = trace_option ~raise (wrong_mini_c_value t v) @@ get_list v in
        let lst' =
          let aux = fun e -> self e ty in
          List.map ~f:aux lst in
        V_List lst'
      )
    | (i, [ty]) when String.equal i set_name -> (
        let lst = trace_option ~raise (wrong_mini_c_value t v) @@ get_set v in
        let lst' =
          let aux = fun e -> self e ty in
          List.map ~f:aux lst in
        V_Set lst'
      )
    | _ ->
      v
  )
  | T_sum {layout ; content} ->
      let lst = List.map ~f:(fun (k,({associated_type;_} : _ row_element_mini_c)) -> (k,associated_type)) @@ Ast_typed.Helpers.kv_list_of_t_sum ~layout content in
      let (Label constructor, v, tv) = Layout.extract_constructor ~raise ~layout v lst in
      let sub = self v tv in
      (V_Construct (constructor, sub))
  | T_record {layout ; content } ->
      let lst = List.map ~f:(fun (k,({associated_type;_} : _ row_element_mini_c)) -> (k,associated_type)) @@ Ast_typed.Helpers.kv_list_of_t_record_or_tuple ~layout content in
      let lst = Layout.extract_record ~raise ~layout v lst in
      let lst = List.Assoc.map ~f:(fun (y, z) -> self y z) lst in
      let m' = Ast_typed.LMap.of_list lst in
      (V_Record m')
  | T_arrow {type1;type2} ->
      (* We now patch the types *)
      let {arg_binder;body} = trace_option ~raise (wrong_mini_c_value t v) @@ get_func v in
      (match body.expression_content with
       | E_application {lamb} ->
          (match lamb.expression_content with
           | E_raw_code {code} ->
              let insertion = e_a_raw_code Stage_common.Backends.michelson code (t_function type1 type2 ()) in
              let body = e_a_application insertion (e_a_variable arg_binder type1) type2 in
              let orig_lambda = e_a_lambda {binder=arg_binder; result=body} type1 type2 in
              V_Func_val {rec_name = None; orig_lambda; arg_binder; body; env = Ligo_interpreter.Environment.empty_env }
           | _ -> v)
       | _ -> v)
  | _ ->
    v

let conv ~raise ~bigmaps (t : Tezos_raw_protocol_011_PtHangzH.Script_repr.expr) (v : Tezos_raw_protocol_011_PtHangzH.Script_repr.expr) =
  let v = v |> Tezos_protocol_011_PtHangzH.Protocol.Michelson_v1_primitives.strings_of_prims
          |> Tezos_micheline.Micheline.inject_locations (fun _ -> ()) in
  let t = t |> Tezos_protocol_011_PtHangzH.Protocol.Michelson_v1_primitives.strings_of_prims
          |> Tezos_micheline.Micheline.inject_locations (fun _ -> ()) in
  decompile_to_untyped_value ~raise ~bigmaps t v
