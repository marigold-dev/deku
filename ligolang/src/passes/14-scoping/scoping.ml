module I = Mini_c
module O = Ligo_coq_ocaml.Ligo
open Ligo_coq_ocaml.Co_de_bruijn
open Tezos_micheline.Micheline

type meta = Location.t

(* We should use this less: *)
let nil = Location.generated

(* Maybe add a field annotation to x, which is expected to be a prim: *)
let annotate (ann : string option) (x : (meta, string) node) : (meta, string) node =
  match ann with
  | None -> x
  | Some ann ->
    match x with
    | Prim (l, p, args, anns) ->
      Prim (l, p, args, ("%"^ann) :: anns)
    | x -> x

(* Next stage uses Micheline for its types: *)
let rec translate_type : I.type_expression -> (meta, string) node =
  fun a ->
  match a.type_content with
  | I.T_tuple ts ->
    tuple_comb ts
  | I.T_or ((ann1, a1), (ann2, a2)) ->
    Prim (nil, "or", [annotate ann1 (translate_type a1);
                      annotate ann2 (translate_type a2)], [])
  | I.T_function (a1, a2) ->
    Prim (nil, "lambda", [translate_type a1; translate_type a2], [])
  | I.T_base I.TB_unit -> Prim (nil, "unit", [], [])
  | I.T_base I.TB_bool -> Prim (nil, "bool", [], [])
  | I.T_base I.TB_string -> Prim (nil, "string", [], [])
  | I.T_base I.TB_bytes -> Prim (nil, "bytes", [], [])
  | I.T_base I.TB_nat -> Prim (nil, "nat", [], [])
  | I.T_base I.TB_int -> Prim (nil, "int", [], [])
  | I.T_base I.TB_mutez -> Prim (nil, "mutez", [], [])
  | I.T_base I.TB_operation -> Prim (nil, "operation", [], [])
  | I.T_base I.TB_address -> Prim (nil, "address", [], [])
  | I.T_base I.TB_key -> Prim (nil, "key", [], [])
  | I.T_base I.TB_key_hash -> Prim (nil, "key_hash", [], [])
  | I.T_base I.TB_chain_id -> Prim (nil, "chain_id", [], [])
  | I.T_base I.TB_signature -> Prim (nil, "signature", [], [])
  | I.T_base I.TB_timestamp -> Prim (nil, "timestamp", [], [])
  | I.T_base I.TB_baker_hash -> Prim (nil, "baker_hash", [], [])
  | I.T_base I.TB_pvss_key -> Prim (nil, "pvss_key", [], [])
  | I.T_base I.TB_baker_operation -> Prim (nil, "baker_operation", [], [])
  | I.T_base I.TB_bls12_381_g1 -> Prim (nil, "bls12_381_g1", [], [])
  | I.T_base I.TB_bls12_381_g2 -> Prim (nil, "bls12_381_g2", [], [])
  | I.T_base I.TB_bls12_381_fr -> Prim (nil, "bls12_381_fr", [], [])
  | I.T_base I.TB_never -> Prim (nil, "never", [], [])
  | I.T_base I.TB_chest -> Prim (nil, "chest", [], [])
  | I.T_base I.TB_chest_key -> Prim (nil, "chest_key", [], [])
  | I.T_ticket x -> Prim (nil, "ticket", [translate_type x], [])
  | I.T_sapling_transaction memo_size -> Prim (nil, "sapling_transaction", [Int (nil, memo_size)], [])
  | I.T_sapling_state memo_size -> Prim (nil, "sapling_state", [Int (nil, memo_size)], [])
  | I.T_map (a1, a2) ->
    Prim (nil, "map", [translate_type a1; translate_type a2], [])
  | I.T_big_map (a1, a2) ->
    Prim (nil, "big_map", [translate_type a1; translate_type a2], [])
  | I.T_list a ->
    Prim (nil, "list", [translate_type a], [])
  | I.T_set a ->
    Prim (nil, "set", [translate_type a], [])
  | I.T_contract a ->
    Prim (nil, "contract", [translate_type a], [])
  | I.T_option a ->
    Prim (nil, "option", [translate_type a], [])

(* could consider delaying this to the next pass, in Coq, but
   currently the Coq pass type translation is the identity *)
and tuple_comb_ann ts =
  match ts with
  | [] -> (None, Prim (nil, "unit", [], []))
  | [(ann, t)] -> (ann, translate_type t)
  | (ann1, t1) :: ts ->
    let t1 = translate_type t1 in
    let (ann, ts) = tuple_comb_ann ts in
    (None, Prim (nil, "pair", [annotate ann1 t1; annotate ann ts], []))

and tuple_comb ts =
  snd (tuple_comb_ann ts)

let translate_var (m : meta) (x : I.var_name) (env : I.environment) =
  let (_, idx) = I.Environment.Environment.get_i x env in
  let usages = List.repeat idx Drop
               @ [ Keep ]
               @ List.repeat (List.length env - idx - 1) Drop in
  (O.E_var m, usages)

let use_nothing env = List.repeat (List.length env) Drop

(* probably should use result monad for conformity? but all errors
   here are supposed to be impossible, under the assumption that the
   input program is well-typed *)
let internal_error loc msg =
  failwith
    (Format.asprintf
       "@[<v>Internal error, please report this as a bug@ %s@ %s@ @]"
       loc msg)

let rec int_to_nat (x : int) : Ligo_coq_ocaml.Datatypes.nat =
  if x <= 0
  then O
  else S (int_to_nat (x - 1))

(* The translation. Given an expression in an environment, returns a
   "co-de Bruijn" expression with an embedding (`list usage`) showing
   which things in the environment were used. *)

(* Let |-I and |-O be the input and output typing judgments. If
   env |-I expr : a, and translate_expression expr env = (expr', us), then
   select us env |-O expr' : a. *)
let rec translate_expression (expr : I.expression) (env : I.environment) =
  let meta = expr.location in
  let ty = expr.type_expression in
  match expr.content with
  | E_literal lit ->
    (O.E_literal (meta, lit), use_nothing env)
  | E_variable x ->
    translate_var meta x env
  | E_closure { binder; body } ->
    let (binder_type, return_type) =
      (* TODO move binder type to the binder, like all other binders? *)
      (* at the moment, this is the only error here! so I am not
         bothering with error machinery... *)
      match Mini_c.get_t_function expr.type_expression with
      | None -> internal_error __LOC__ "type of lambda is not a function type"
      | Some t -> t in
    let binder = (binder, binder_type) in
    let (binder, usages) = translate_binder (binder, body) env in
    (O.E_lam (meta, binder, translate_type return_type), usages)
  | E_constant constant ->
    let ((cons_name, static_args, args), usages) = translate_constant constant expr.type_expression env in
    (O.E_operator (meta, cons_name, static_args, args), usages)
  | E_application (f, x) ->
    let (args, us) = translate_args [f; x] env in
    (E_app (meta, args), us)
  | E_iterator (name, body, expr) ->
    let (body, body_usages) = translate_binder body env in
    let (expr, expr_usages) = translate_expression expr env in
    let (ss, us) = union body_usages expr_usages in
    (match name with
     | C_ITER -> (O.E_iter (meta, ss, body, expr), us)
     | C_MAP -> (O.E_map (meta, ss, body, expr), us)
     | C_LOOP_LEFT ->
       let b = translate_type ty in
       (O.E_loop_left (meta, ss, body, b, expr), us)
     | _ -> internal_error __LOC__ "invalid iterator constant")
  | E_fold (body, coll, init) ->
    let (body, body_usages) = translate_binder body env in
    let (coll, coll_usages) = translate_expression coll env in
    let (init, init_usages) = translate_expression init env in
    let (ss1, us1) = union coll_usages body_usages in
    let (ss2, us2) = union init_usages us1 in
    (O.E_fold (meta, ss2, init, ss1, coll, body), us2)
  | E_fold_right (body, (coll, elem_type), init) ->
    let elem_type = translate_type elem_type in
    let (body, body_usages) = translate_binder body env in
    let (coll, coll_usages) = translate_expression coll env in
    let (init, init_usages) = translate_expression init env in
    let (ss1, us1) = union coll_usages body_usages in
    let (ss2, us2) = union init_usages us1 in
    (O.E_fold_right (meta, elem_type, ss2, init, ss1, coll, body), us2)
  | E_if_bool (e1, e2, e3) ->
    let (e1, us1) = translate_expression e1 env in
    let (e2, us2) = translate_expression e2 env in
    let (e3, us3) = translate_expression e3 env in
    let (inner, inner_us) = union us2 us3 in
    let (outer, outer_us) = union us1 inner_us in
    (E_if_bool (meta, Cond (outer, e1, inner, Binds ([], [], e2), Binds ([], [], e3))), outer_us)
  | E_if_none (e1, e2, e3) ->
    let (e1, us1) = translate_expression e1 env in
    let (e2, us2) = translate_expression e2 env in
    let (e3, us3) = translate_binder e3 env in
    let (inner, inner_us) = union us2 us3 in
    let (outer, outer_us) = union us1 inner_us in
    (E_if_none (meta, Cond (outer, e1, inner, Binds ([], [], e2), e3)), outer_us)
  (* NB: flipping around because it is backwards in Mini_c *)
  | E_if_cons (e1, e3, e2) ->
    let (e1, us1) = translate_expression e1 env in
    let (e2, us2) = translate_binder2 e2 env in
    let (e3, us3) = translate_expression e3 env in
    let (inner, inner_us) = union us2 us3 in
    let (outer, outer_us) = union us1 inner_us in
    (E_if_cons (meta, Cond (outer, e1, inner, e2, Binds([], [], e3))), outer_us)
  | E_if_left (e1, e2, e3) ->
    let (e1, us1) = translate_expression e1 env in
    let (e2, us2) = translate_binder e2 env in
    let (e3, us3) = translate_binder e3 env in
    let (inner, inner_us) = union us2 us3 in
    let (outer, outer_us) = union us1 inner_us in
    (E_if_left (meta, Cond (outer, e1, inner, e2, e3)), outer_us)
  | E_let_in (e1, _inline, e2) ->
    let (e1, us1) = translate_expression e1 env in
    let (e2, us2) = translate_binder e2 env in
    let (ss, us) = union us1 us2 in
    (E_let_in (meta, ss, e1, e2), us)
  | E_tuple exprs ->
    (* arguments are in reverse order for REV_PAIR for now *)
    let (exprs, us) = translate_args (List.rev exprs) env in
    (E_tuple (meta, exprs), us)
  | E_let_tuple (e1, e2) ->
    let (e1, us1) = translate_expression e1 env in
    let (e2, us2) = translate_binderN e2 env in
    let (ss, us) = union us1 us2 in
    (E_let_tuple (meta, ss, e1, e2), us)
  | E_proj (e, i, n) ->
    let (e, us) = translate_expression e env in
    (E_proj (meta, e, int_to_nat i, int_to_nat n), us)
  | E_update (e1, i, e2, n) ->
    let (args, us) = translate_args [e2; e1] env in
    (E_update (meta, args, int_to_nat i, int_to_nat n), us)
  | E_raw_michelson code ->
    (* maybe should move type into syntax? *)
    let (a, b) = match Mini_c.get_t_function ty with
      | None -> internal_error __LOC__ "type of Michelson insertion ([%Michelson ...]) is not a function type"
      | Some (a, b) -> (a, b) in
    (E_raw_michelson (meta, translate_type a, translate_type b, code), use_nothing env)

and translate_binder (binder, body) env =
  let env' = I.Environment.add binder env in
  let (body, usages) = translate_expression body env' in
  let (_, binder_type) = binder in
  (O.Binds ([List.hd_exn usages], [translate_type binder_type], body), List.tl_exn usages)

and translate_binder2 ((binder1, binder2), body) env =
  let env' = I.Environment.add binder1 (I.Environment.add binder2 env) in
  let (body, usages) = translate_expression body env' in
  let (_, binder1_type) = binder1 in
  let (_, binder2_type) = binder2 in
  (O.Binds ([List.hd_exn usages; List.hd_exn (List.tl_exn usages)],
            [translate_type binder1_type; translate_type binder2_type],
            body),
   List.tl_exn (List.tl_exn usages))

and translate_binderN (vars, body) env =
  let env' = List.fold_right ~f:I.Environment.add vars ~init:env in
  let (body, usages) = translate_expression body env' in
  let var_types = List.map ~f:snd vars in
  let n = List.length vars in
  (O.Binds (List.take usages n,
            List.map ~f:translate_type var_types,
            body),
   List.drop usages n)

and translate_args (arguments : I.expression list) env : _ O.args * usage list =
  let arguments = List.rev arguments in
  let arguments = List.map ~f:(fun argument -> translate_expression argument env) arguments in
  List.fold_right
    ~f:(fun (arg, arg_usages) (args, args_usages) ->
       let (ss, us) = union arg_usages args_usages in
       (O.Args_cons (ss, arg, args), us))
    arguments
    ~init:(O.Args_nil, use_nothing env)

and translate_constant (expr : I.constant) (ty : I.type_expression) env :
  (Stage_common.Types.constant' * _ O.static_args * _ O.args) * usage list =
  let module Let_syntax = struct
    let bind : 'a option -> f:('a -> 'b option) -> 'b option =
      fun x ~f ->
        match x with
        | Some x -> f x
        | None -> None
    end in
  let (let*) x f = Let_syntax.bind ~f x in
  (* Here we will handle some special predefined operators which take
     some "static args". These are mostly types from the typing
     judgment, but also annotations (for SELF, CONTRACT) or scripts
     (for CREATE_CONTRACT.)

     First we translate any static args and return the rest of the
     non-static arguments, if any: *)
  let special : (_ O.static_args * I.expression list) option =
    let return (x : _ O.static_args * I.expression list) : _ = Some x in
    match expr.cons_name with
    | C_VIEW -> (
      match expr.arguments with
      | { content = E_literal (Literal_string view_name); type_expression = _ } :: arguments ->
        let* view_ret_t = Mini_c.get_t_option ty in
        let view_name = Ligo_string.extract view_name in
        return (Type_args (None, [String (nil, view_name) ; translate_type view_ret_t]), arguments)
      | _ -> None
    )
    | C_SELF -> (
      match expr.arguments with
      | { content = E_literal (Literal_string annot) ; _ } :: arguments ->
        let annot = Ligo_string.extract annot in
        return (Type_args (Some annot, []), arguments)
      | _ -> None
    )
    | C_NONE | C_BYTES_UNPACK ->
      let* a = Mini_c.get_t_option ty in
      return (Type_args (None, [translate_type a]), expr.arguments)
    | C_NIL | C_LIST_EMPTY ->
      let* a = Mini_c.get_t_list ty in
      return (Type_args (None, [translate_type a]), expr.arguments)
    | C_LOOP_CONTINUE | C_LEFT ->
      let* (_, b) = Mini_c.get_t_or ty in
      return (Type_args (None, [translate_type b]), expr.arguments)
    | C_LOOP_STOP | C_RIGHT ->
      let* (a, _) = Mini_c.get_t_or ty in
      return (Type_args (None, [translate_type a]), expr.arguments)
    | C_SET_EMPTY ->
      let* a = Mini_c.get_t_set ty in
      return (Type_args (None, [translate_type a]), expr.arguments)
    | C_MAP_EMPTY | C_BIG_MAP_EMPTY ->
      let* (a, b) =
        Option.(map_pair_or (Mini_c.get_t_map , Mini_c.get_t_big_map) ty) in
      return (Type_args (None, [translate_type a; translate_type b]), expr.arguments)
    | C_MAP_REMOVE ->
      let* (_, b) =
        Option.(map_pair_or (Mini_c.get_t_map , Mini_c.get_t_big_map) ty) in
      return (Type_args (None, [translate_type b]), expr.arguments)
    | C_LIST_HEAD_OPT | C_LIST_TAIL_OPT ->
      let* a = Mini_c.get_t_option ty in
      return (Type_args (None, [translate_type a]), expr.arguments)
    | C_CONTRACT | C_CONTRACT_WITH_ERROR ->
      let* a = Mini_c.get_t_contract ty in
      return (Type_args (None, [translate_type a]), expr.arguments)
    | C_CONTRACT_OPT ->
      let* a = Mini_c.get_t_option ty in
      let* a = Mini_c.get_t_contract a in
      Some (O.Type_args (None, [translate_type a]), expr.arguments)
    | C_CONTRACT_ENTRYPOINT ->
      let* a = Mini_c.get_t_contract ty in
      (match expr.arguments with
       | { content = E_literal (Literal_string annot); type_expression = _ } :: arguments ->
         let annot = Ligo_string.extract annot in
         return (O.Type_args (Some annot, [translate_type a]), arguments)
       | _ -> None)
    | C_CONTRACT_ENTRYPOINT_OPT ->
      let* a = Mini_c.get_t_option ty in
      let* a = Mini_c.get_t_contract a in
      (match expr.arguments with
       | { content = E_literal (Literal_string annot); type_expression = _ } :: arguments ->
         let annot = Ligo_string.extract annot in
         return (O.Type_args (Some annot, [translate_type a]), arguments)
       | _ -> None)
    | C_CREATE_CONTRACT ->
      (match expr.arguments with
       | { content= E_closure body ; type_expression = closure_ty } :: arguments ->
         let* (input_ty, _) = Mini_c.get_t_function closure_ty in
         let* (p, s) = Mini_c.get_t_pair input_ty in
         let body = translate_closed_function body input_ty in
         return (O.Script_arg (O.Script (translate_type p, translate_type s, body)), arguments)
       | _ -> None)
    | C_SAPLING_EMPTY_STATE ->
      let* memo_size = Mini_c.get_t_sapling_state ty in
      return (Type_args (None, [Int (nil, memo_size)]), expr.arguments)
    | _ -> None in
  (* Either we got static args, or none: *)
  let static_args = match special with
    | Some (static_args, _) -> static_args
    | None -> O.Type_args (None, []) in
  (* Remaining/all non-static args: *)
  let arguments = match special with
    | Some (_, arguments) -> arguments
    | None -> expr.arguments in
  let (arguments, usages) = translate_args arguments env in
  ((expr.cons_name, static_args, arguments), usages)

and translate_closed_function ({ binder ; body } : I.anon_function) input_ty : _ O.binds =
  let (body, usages) = translate_expression body (Mini_c.Environment.add (binder, input_ty) []) in
  Binds (usages, [translate_type input_ty], body)
