open Trace
open Ligo_interpreter.Types
open Ligo_interpreter.Combinators
include Ast_typed.Types
module Env = Ligo_interpreter.Environment

type interpreter_error = Errors.interpreter_error

let check_value value =
  let open Monad in
  match value with
  | V_Func_val {orig_lambda} ->
     call @@ Check_obj_ligo orig_lambda
  | _ -> return ()

let monad_option error = fun v ->
    let open Monad in
    match v with
      None -> fail error
    | Some s -> return s

let wrap_compare_result comp cmpres loc calltrace =
  let open Monad in
  match comp with
  | C_EQ -> return (cmpres = 0)
  | C_NEQ -> return (cmpres <> 0)
  | C_LT -> return (cmpres < 0)
  | C_LE -> return (cmpres <= 0)
  | C_GT -> return (cmpres > 0)
  | C_GE -> return (cmpres >= 0)
  | _ -> fail @@ Errors.meta_lang_eval loc calltrace "Not comparable"

let compare_constants c o1 o2 loc calltrace =
  let open Monad in
  match (c, [o1; o2]) with
  | (comp, [V_Ct (C_int a'); V_Ct (C_int b')])
  | (comp, [V_Ct (C_mutez a'); V_Ct (C_mutez b')])
  | (comp, [V_Ct (C_timestamp a'); V_Ct (C_timestamp b')])
  | (comp, [V_Ct (C_nat a'); V_Ct (C_nat b')]) ->
      let cmpres = Z.compare a' b' in
      let* x = wrap_compare_result comp cmpres loc calltrace in
      return @@ v_bool x
  | (comp, [V_Ct (C_bool b); V_Ct (C_bool a)]) ->
      let cmpres = Bool.compare b a in
      let* x = wrap_compare_result comp cmpres loc calltrace in
      return @@ v_bool x
  | (comp, [V_Ct (C_address b); V_Ct (C_address a)]) ->
      let cmpres = Tezos_state.compare_account_ b a in
      let* x = wrap_compare_result comp cmpres loc calltrace in
      return @@ v_bool x
  | (comp, [V_Ct (C_key_hash b); V_Ct (C_key_hash a)]) ->
      let cmpres = Tezos_crypto.Signature.Public_key_hash.compare b a in
      let* x = wrap_compare_result comp cmpres loc calltrace in
      return @@ v_bool x
  | (comp, [V_Ct C_unit; V_Ct C_unit]) ->
      let* x =
        match comp with
        | C_EQ -> return true
        | C_NEQ -> return false
        | C_LT -> return false
        | C_LE -> return true
        | C_GT -> return false
        | C_GE -> return true
        | _ -> fail @@ Errors.meta_lang_eval loc calltrace "Not comparable"
      in
      return @@ v_bool x
  | (comp, [V_Ct (C_string a'); V_Ct (C_string b')]) ->
      let* f_cmp = return @@ fun a b -> String.compare a b in
      let* cmpres = return @@ f_cmp a' b' in
      let* x = wrap_compare_result comp cmpres loc calltrace in
      Monad.return @@ v_bool x
  | (comp, [V_Ct (C_bytes a'); V_Ct (C_bytes b')]) ->
      let* f_cmp = return @@ fun a b -> Bytes.compare a b in
      let* cmpres = return @@ f_cmp a' b' in
      let* x = wrap_compare_result comp cmpres loc calltrace in
      Monad.return @@ v_bool x
  | ( comp,
      [
        V_Ct (C_contract {address = addr1; entrypoint = entr1});
        V_Ct (C_contract {address = addr2; entrypoint = entr2});
      ] ) ->
      let compare_opt_strings o1 o2 =
        match (o1, o2) with (Some s1, Some s2) -> s1 = s2 | _ -> false
      in
      let cmpres = Tezos_state.compare_account_ addr1 addr2 in
      let* x =
        match comp with
        | C_EQ -> return (cmpres = 0 && compare_opt_strings entr1 entr2)
        | C_NEQ -> return (cmpres <> 0 && compare_opt_strings entr1 entr2)
        | _ -> fail @@ Errors.meta_lang_eval loc calltrace "Not comparable"
      in
      return @@ v_bool x
  | (_, l) ->
      print_endline
        (Format.asprintf
            "%a"
            (PP_helpers.list_sep_d Ligo_interpreter.PP.pp_value)
            l) ;
      fail @@ Errors.meta_lang_eval loc calltrace "Not comparable"

let rec apply_comparison :
    Location.t ->
    calltrace ->
    Ast_typed.constant' ->
    value list ->
    value Monad.t =
  fun loc calltrace c operands ->
  let open Monad in
  match (c, operands) with
  | (C_EQ  , [ (V_Michelson _) as a ; (V_Michelson _) as b ] ) ->
    let>> b = Michelson_equal (loc,a,b) in
    return @@ v_bool b
  | (C_NEQ  , [ (V_Michelson _) as a ; (V_Michelson _) as b ] ) ->
    let>> b = Michelson_equal (loc,a,b) in
    return @@ v_bool (not b)
  | (comp, [(V_Ct _ as v1); (V_Ct _ as v2)]) ->
      compare_constants comp v1 v2 loc calltrace
  | (comp, [V_Ligo (a1, b1); V_Ligo (a2, b2)]) ->
      let* x =
        match comp with
        | C_EQ -> return (a1 = a2 && b1 = b2)
        | C_NEQ -> return (a1 = a2 && b1 = b2)
        | _ -> fail @@ Errors.meta_lang_eval loc calltrace "Not comparable"
      in
      return @@ v_bool x
  | (comp, [V_List   _ as xs; V_List   _ as ys])
  | (comp, [V_Set    _ as xs; V_Set    _ as ys])
  | (comp, [V_Map    _ as xs; V_Map    _ as ys])
  | (comp, [V_Record _ as xs; V_Record _ as ys]) ->
    let c = Ligo_interpreter.Combinators.equal_value xs ys in
    let* v =
      match comp with
      | C_EQ  -> return @@ v_bool c
      | C_NEQ -> return @@ v_bool (not c)
      | _ -> fail @@ Errors.meta_lang_eval loc calltrace "Not comparable"
    in
    return v
  | (comp, [V_Construct (ctor_a, args_a); V_Construct (ctor_b, args_b)]) -> (
      match comp with
      | C_EQ ->
          if String.equal ctor_a ctor_b then
            let* r = apply_comparison loc calltrace c [args_a; args_b] in
            Monad.return @@ v_bool @@ is_true r
          else Monad.return @@ v_bool false
      | C_NEQ ->
          if not (String.equal ctor_a ctor_b) then Monad.return @@ v_bool true
          else
            let* r = apply_comparison loc calltrace c [args_a; args_b] in
            Monad.return @@ v_bool @@ is_true r
      | _ -> fail @@ Errors.meta_lang_eval loc calltrace "Not comparable")
  | (_, l) ->
    (* TODO: Don't know how to compare these *)
      (* V_Func_val *)
      (* V_Mutation *)
      (* V_Failure *)
      (* V_Michelson *)
      (* V_BigMap *)
      print_endline
        (Format.asprintf
            "%a"
            (PP_helpers.list_sep_d Ligo_interpreter.PP.pp_value)
            l) ;
      fail @@ Errors.meta_lang_eval loc calltrace "Not comparable"

let rec apply_operator ~raise ~steps ~protocol_version : Location.t -> calltrace -> Ast_typed.type_expression -> env -> Ast_typed.constant' -> (value * Ast_typed.type_expression) list -> value Monad.t =
  fun loc calltrace expr_ty env c operands ->
  let open Monad in
  let eval_ligo = eval_ligo ~raise ~steps ~protocol_version in
  let types = List.map ~f:snd operands in
  let operands = List.map ~f:fst operands in
  let return_ct v = return @@ V_Ct v in
  let return_none () = return @@ v_none () in
  let return_some v = return @@ v_some v in
  ( match (c,operands) with
    (* nullary *)
    | ( C_NONE , [] ) -> return_none ()
    | ( C_UNIT , [] ) -> return @@ V_Ct C_unit
    | ( C_NIL  , [] ) -> return @@ V_List []
    (* unary *)
    | ( C_SIZE   , [(V_Set l | V_List l)] ) -> return_ct @@ C_nat (Z.of_int @@ List.length l)
    | ( C_SIZE   , [ V_Map l            ] ) -> return_ct @@ C_nat (Z.of_int @@ List.length l)
    | ( C_SIZE   , [ V_Ct (C_string s ) ] ) -> return_ct @@ C_nat (Z.of_int @@ String.length s)
    | ( C_SIZE   , [ V_Ct (C_bytes b  ) ] ) -> return_ct @@ C_nat (Z.of_int @@ Bytes.length b)
    | ( C_NOT    , [ V_Ct (C_bool a'  ) ] ) -> return_ct @@ C_bool (not a')
    (* TODO-er: fix two complements: *)
    | ( C_NOT    , [ V_Ct (C_int a'   ) ] ) -> return_ct @@ C_int (Z.neg a')
    | ( C_NOT    , [ V_Ct (C_nat a'   ) ] ) -> return_ct @@ C_int (Z.neg a')
    | ( C_INT    , [ V_Ct (C_nat a')    ] ) -> return_ct @@ C_int a'
    | ( C_ABS    , [ V_Ct (C_int a')    ] ) -> return_ct @@ C_nat (Z.abs a')
    | ( C_NEG    , [ V_Ct (C_int a')    ] ) -> return_ct @@ C_int (Z.neg a')
    | ( C_SOME   , [ v                  ] ) -> return_some v
    | ( C_IS_NAT , [ V_Ct (C_int a')    ] ) ->
      if a' > Z.zero then return_some @@ V_Ct (C_nat a')
      else return_none ()
    | ( C_FOLD_CONTINUE  , [ v ] ) -> return @@ v_pair (v_bool true  , v)
    | ( C_FOLD_STOP      , [ v ] ) -> return @@ v_pair (v_bool false , v)
    | ( C_ASSERTION , [ v ] ) ->
      if (is_true v) then return_ct @@ C_unit
      else fail @@ Errors.meta_lang_eval loc calltrace "Failed assertion"
    | C_MAP_FIND_OPT , [ k ; V_Map l ] -> ( match List.Assoc.find ~equal:LC.equal_value l k with
      | Some v -> return @@ v_some v
      | None -> return @@ v_none ()
    )
    | C_MAP_FIND , [ k ; V_Map l ] -> ( match List.Assoc.find ~equal:LC.equal_value l k with
      | Some v -> return @@ v
      | None -> fail @@ Errors.meta_lang_eval loc calltrace (Predefined.Tree_abstraction.pseudo_module_to_string c)
    )
    (* binary *)
    | ( (C_EQ | C_NEQ | C_LT | C_LE | C_GT | C_GE) , _ ) -> apply_comparison loc calltrace c operands
    | ( C_SUB    , [ V_Ct (C_int a' | C_nat a') ; V_Ct (C_int b' | C_nat b') ] ) -> return_ct @@ C_int (Z.sub a' b')
    | ( C_SUB    , [ V_Ct (C_int a' | C_timestamp a') ; V_Ct (C_timestamp b' | C_int b') ] ) ->
      let res = Michelson_backend.Tezos_eq.timestamp_sub a' b' in
      return_ct @@ C_timestamp res
    | ( C_SUB    , [ V_Ct (C_mutez a') ; V_Ct (C_mutez b') ] ) -> (
      match Michelson_backend.Tezos_eq.mutez_sub a' b' with
      | Some res -> return_ct @@ C_mutez res
      | None -> fail (Errors.meta_lang_eval loc calltrace "Mutez underflow/overflow")
    )
    | ( C_CONS   , [ v                  ; V_List vl          ] ) -> return @@ V_List (v::vl)
    | ( C_ADD    , [ V_Ct (C_int a  )  ; V_Ct (C_int b  )  ] )
    | ( C_ADD    , [ V_Ct (C_nat a  )  ; V_Ct (C_int b  )  ] )
    | ( C_ADD    , [ V_Ct (C_int a  )  ; V_Ct (C_nat b  )  ] ) -> let r = Z.add a b in return_ct (C_int r)
    | ( C_ADD    , [ V_Ct (C_nat a  )  ; V_Ct (C_nat b  )  ] ) -> let r = Z.add a b in return_ct (C_nat r)
    | ( C_ADD    , [ V_Ct (C_int a' | C_timestamp a') ; V_Ct (C_timestamp b' | C_int b') ] ) ->
      let res = Michelson_backend.Tezos_eq.timestamp_add a' b' in
      return_ct @@ C_timestamp res
    | ( C_ADD    , [ V_Ct (C_mutez a') ; V_Ct (C_mutez b') ] ) -> (
      match Michelson_backend.Tezos_eq.mutez_add a' b' with
      | Some res -> return_ct @@ C_mutez res
      | None -> fail (Errors.meta_lang_eval loc calltrace "Mutez underflow/overflow")
    )
    | ( C_MUL    , [ V_Ct (C_int a  )  ; V_Ct (C_int b  )  ] )
    | ( C_MUL    , [ V_Ct (C_nat a  )  ; V_Ct (C_int b  )  ] )
    | ( C_MUL    , [ V_Ct (C_int a  )  ; V_Ct (C_nat b  )  ] ) -> let r = Z.mul a b in return_ct (C_int r)
    | ( C_MUL    , [ V_Ct (C_nat a  )  ; V_Ct (C_nat b  )  ] ) -> let r = Z.mul a b in return_ct (C_nat r)
    | ( C_MUL    , [ V_Ct (C_nat a  )  ; V_Ct (C_mutez b)  ] ) -> let r = Z.mul a b in return_ct (C_mutez r)
    | ( C_MUL    , [ V_Ct (C_mutez a)  ; V_Ct (C_nat b  )  ] ) -> let r = Z.mul a b in return_ct (C_mutez r)
    | ( C_DIV    , [ V_Ct (C_int a'  )  ; V_Ct (C_int b'  )  ] )
    | ( C_DIV    , [ V_Ct (C_int a'  )  ; V_Ct (C_nat b'  )  ] )
    | ( C_DIV    , [ V_Ct (C_nat a'  )  ; V_Ct (C_int b'  )  ] ) ->
      let a = Michelson_backend.Tezos_eq.int_ediv a' b' in
      begin
        match a with
        | Some (res,_) -> return_ct @@ C_int res
        | None -> fail @@ Errors.meta_lang_eval loc calltrace "Dividing by zero"
      end
    | ( C_DIV    , [ V_Ct (C_nat a')  ; V_Ct (C_nat b')  ] ) ->
      let a = Michelson_backend.Tezos_eq.int_ediv a' b' in
      begin
        match a with
        | Some (res,_) -> return_ct @@ C_nat res
        | None -> fail @@ Errors.meta_lang_eval loc calltrace "Dividing by zero"
      end
    | ( C_DIV    , [ V_Ct (C_mutez a')  ; V_Ct (C_mutez b')  ] ) ->
      let a = Michelson_backend.Tezos_eq.int_ediv a' b' in
      begin
        match a with
        | Some (res,_) -> return_ct @@ C_nat res
        | None -> fail @@ Errors.meta_lang_eval loc calltrace "Dividing by zero"
      end
    | ( C_DIV    , [ V_Ct (C_mutez a')  ; V_Ct (C_nat b')  ] ) ->
      let a = Michelson_backend.Tezos_eq.int_ediv a' b' in
      begin
        match a with
        | Some (res,_) -> return_ct @@ C_mutez res
        | None -> fail @@ Errors.meta_lang_eval loc calltrace "Dividing by zero"
      end
    | ( C_MOD    , [ V_Ct (C_int a')    ; V_Ct (C_int b')    ] )
    | ( C_MOD    , [ V_Ct (C_int a')    ; V_Ct (C_nat b')    ] )
    | ( C_MOD    , [ V_Ct (C_nat a')    ; V_Ct (C_int b')    ] ) -> (
      let a = Michelson_backend.Tezos_eq.int_ediv a' b' in
      match a with
      | Some (_,r) -> return_ct @@ C_nat r
      | None -> fail @@ Errors.meta_lang_eval loc calltrace "Dividing by zero"
    )
    | ( C_MOD    , [ V_Ct (C_nat a')    ; V_Ct (C_nat b')    ] ) -> (
      let a = Michelson_backend.Tezos_eq.int_ediv a' b' in
      match a with
      | Some (_,r) -> return_ct @@ C_nat r
      | None -> fail @@ Errors.meta_lang_eval loc calltrace "Dividing by zero"
    )
    | ( C_CONCAT , [ V_Ct (C_string a') ; V_Ct (C_string b') ] ) -> return_ct @@ C_string (a' ^ b')
    | ( C_CONCAT , [ V_Ct (C_bytes a' ) ; V_Ct (C_bytes b' ) ] ) -> return_ct @@ C_bytes  (Bytes.cat a' b')
    | ( C_OR     , [ V_Ct (C_bool a'  ) ; V_Ct (C_bool b'  ) ] ) -> return_ct @@ C_bool   (a' || b')
    | ( C_AND    , [ V_Ct (C_bool a'  ) ; V_Ct (C_bool b'  ) ] ) -> return_ct @@ C_bool   (a' && b')
    | ( C_XOR    , [ V_Ct (C_bool a'  ) ; V_Ct (C_bool b'  ) ] ) -> return_ct @@ C_bool   ( (a' || b') && (not (a' && b')) )
    (* Bitwise operators *)
    | ( C_AND    , [ V_Ct (C_int a'  ) ; V_Ct (C_nat b'  ) ] ) -> let v = Z.logand a' b' in return_ct @@ C_nat v
    | ( C_AND    , [ V_Ct (C_nat a'  ) ; V_Ct (C_nat b'  ) ] ) -> let v = Z.logand a' b' in return_ct @@ C_nat v
    | ( C_OR     , [ V_Ct (C_nat a'  ) ; V_Ct (C_nat b'  ) ] ) -> let v = Z.logor a' b' in return_ct @@ C_nat v
    | ( C_XOR    , [ V_Ct (C_nat a'  ) ; V_Ct (C_nat b'  ) ] ) -> let v = Z.logxor a' b' in return_ct @@ C_nat v
    | ( C_LSL    , [ V_Ct (C_nat a'  ) ; V_Ct (C_nat b'  ) ] ) ->
      let v = Michelson_backend.Tezos_eq.nat_shift_left a' b' in
      begin
        match v with
        | Some v -> return_ct @@ C_nat v
        | None -> fail @@ Errors.meta_lang_eval loc calltrace "Overflow"
      end
    | ( C_LSR    , [ V_Ct (C_nat a'  ) ; V_Ct (C_nat b'  ) ] ) ->
      let v = Michelson_backend.Tezos_eq.nat_shift_right a' b' in
      begin
        match v with
        | Some v -> return_ct @@ C_nat v
        | None -> fail @@ Errors.meta_lang_eval loc calltrace "Overflow"
      end
    | ( C_LIST_EMPTY, []) -> return @@ V_List ([])
    | ( C_LIST_MAP , [ V_Func_val {arg_binder ; body ; env}  ; V_List (elts) ] ) ->
      let* lst_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 1 in
      let* ty = monad_option (Errors.generic_error lst_ty.location "Expected list type") @@ Ast_typed.get_t_list lst_ty in
      let* elts =
        Monad.bind_map_list
          (fun elt ->
            let env' = Env.extend env arg_binder (ty,elt) in
            eval_ligo body calltrace env')
          elts
      in
      return (V_List elts)
    | ( C_MAP_MAP , [ V_Func_val {arg_binder ; body ; env }  ; V_Map (elts) ] ) ->
      let* map_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 1 in
      let* k_ty,v_ty = monad_option (Errors.generic_error map_ty.location "Expected map type") @@ Ast_typed.get_t_map map_ty in
      let* elts =
        Monad.bind_map_list
          (fun (k,v) ->
            let env' = Env.extend env arg_binder ((Ast_typed.t_pair k_ty v_ty),v_pair (k,v)) in
            let* v' = eval_ligo body calltrace env' in
            return @@ (k,v')
          )
          elts
      in
      return (V_Map elts)
    | ( C_LIST_ITER , [ V_Func_val {arg_binder ; body ; env}  ; V_List (elts) ] ) ->
      let* lst_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 1 in
      let* ty = monad_option (Errors.generic_error lst_ty.location "Expected list type") @@ Ast_typed.get_t_list lst_ty in
      Monad.bind_fold_list
        (fun _ elt ->
          let env' = Env.extend env arg_binder (ty,elt) in
          eval_ligo body calltrace env'
        )
        (V_Ct C_unit) elts
    | ( C_MAP_ITER , [ V_Func_val {arg_binder ; body ; env}  ; V_Map (elts) ] ) ->
      let* map_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 1 in
      let* k_ty,v_ty = monad_option (Errors.generic_error map_ty.location "Expected map type") @@ Ast_typed.get_t_map map_ty in
      Monad.bind_fold_list
        (fun _ kv ->
          let env' = Env.extend env arg_binder ((Ast_typed.t_pair k_ty v_ty),v_pair kv) in
          eval_ligo body calltrace env'
        )
        (V_Ct C_unit) elts
    | ( C_FOLD_WHILE , [ V_Func_val {arg_binder ; body ; env}  ; init ] ) -> (
      let* arg_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 1 in
      let rec aux b el =
        let env' = Env.extend env arg_binder (arg_ty, el) in
        let* res = eval_ligo body calltrace env' in
        let (b',el') = try Option.value_exn (extract_fold_while_result res) with _ -> (failwith "bad pair") in
        if b then aux b' el' else return el' in
      aux true init
    )
    (* tertiary *)
    | ( C_SLICE , [ V_Ct (C_nat st) ; V_Ct (C_nat ed) ; V_Ct (C_string s) ] ) ->
      (*TODO : allign with tezos*)
      return @@ V_Ct (C_string (String.sub s (Z.to_int st) (Z.to_int ed)))
    | ( C_LIST_FOLD_LEFT , [ V_Func_val {arg_binder ; body ; env}  ; init ; V_List elts ] ) ->
      let* lst_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 2 in
      let* acc_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 1 in
      let* ty = monad_option (Errors.generic_error lst_ty.location "Expected list type") @@ Ast_typed.get_t_list lst_ty in
      Monad.bind_fold_list
        (fun prev elt ->
          let fold_args = v_pair (prev,elt) in
          let env' = Env.extend env arg_binder ((Ast_typed.t_pair acc_ty ty), fold_args) in
          eval_ligo body calltrace env'
        )
        init elts
    | ( C_FOLD , [ V_Func_val {arg_binder ; body ; env}  ; V_List elts ; init ] )
    | ( C_LIST_FOLD , [ V_Func_val {arg_binder ; body ; env}  ; V_List elts ; init ] ) ->
      let* lst_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 1 in
      let* acc_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 2 in
      let* ty = monad_option (Errors.generic_error lst_ty.location "Expected list type") @@ Ast_typed.get_t_list lst_ty in
      Monad.bind_fold_list
        (fun prev elt ->
          let fold_args = v_pair (prev,elt) in
          let env' = Env.extend env arg_binder ((Ast_typed.t_pair acc_ty ty), fold_args) in
          eval_ligo body calltrace env'
        )
        init elts
    | ( C_LIST_FOLD_RIGHT , [ V_Func_val {arg_binder ; body ; env}  ; V_List elts ; init ] ) ->
      let* lst_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 1 in
      let* acc_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 2 in
      let* ty = monad_option (Errors.generic_error lst_ty.location "Expected list type") @@ Ast_typed.get_t_list lst_ty in
      Monad.bind_fold_right_list
        (fun elt prev ->
          let fold_args = v_pair (elt,prev) in
          let env' = Env.extend env arg_binder ((Ast_typed.t_pair ty acc_ty), fold_args) in
          eval_ligo body calltrace env'
        )
        init elts
    | ( C_LIST_HEAD_OPT , [ V_List elts ] ) ->
      (match (List.hd elts) with
      | Some v -> return @@ v_some v
      | None   -> return @@ v_none ())
    | ( C_LIST_TAIL_OPT , [ V_List elts ] ) ->
      (match (List.tl elts) with
      | Some v -> return @@ v_some (V_List v)
      | None   -> return @@ v_none ())
    | ( C_BIG_MAP_EMPTY , []) -> return @@ V_Map ([])
    | ( C_MAP_EMPTY , []) -> return @@ V_Map ([])
    | ( C_MAP_FOLD , [ V_Func_val {arg_binder ; body ; env}  ; V_Map kvs ; init ] ) ->
      let* map_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 1 in
      let* acc_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 2 in
      let* k_ty,v_ty = monad_option (Errors.generic_error map_ty.location "Expected map type") @@ Ast_typed.get_t_map map_ty in
      Monad.bind_fold_list
        (fun prev kv ->
          let fold_args = v_pair (prev, v_pair kv) in
          let env' = Env.extend env arg_binder (Ast_typed.(t_pair acc_ty (t_pair k_ty v_ty)),  fold_args) in
          eval_ligo body calltrace env'
        )
        init kvs
    | ( C_MAP_MEM , [k ; V_Map kvs]) -> return @@ v_bool (List.Assoc.mem ~equal:LC.equal_value kvs k)
    | ( C_MAP_ADD , [ k ; v ; V_Map kvs] ) -> return (V_Map ((k,v) :: List.Assoc.remove ~equal:LC.equal_value kvs k))
    | ( C_MAP_REMOVE , [ k ; V_Map kvs] ) -> return @@ V_Map (List.Assoc.remove ~equal:LC.equal_value kvs k)
    | ( C_MAP_UPDATE , [ k ; V_Construct (option,v) ; V_Map kvs] ) -> (match option with
      | "Some" -> return @@ V_Map ((k,v)::(List.Assoc.remove ~equal:LC.equal_value kvs k))
      | "None" -> return @@ V_Map (List.Assoc.remove ~equal:LC.equal_value kvs k)
      | _ -> assert false
    )
    | ( C_MAP_GET_AND_UPDATE , [k ; V_Construct (option,v) ; V_Map kvs ] ) ->
      let old_value = List.Assoc.find ~equal:LC.equal_value kvs k in
      let old_value = (match old_value with
        | Some v -> v_some v
        | None -> v_none ())
      in
      (match option with
      | "Some" -> return @@ v_pair (old_value, V_Map ((k,v)::(List.Assoc.remove ~equal:LC.equal_value kvs k)))
      | "None" -> return @@ v_pair (old_value, V_Map (List.Assoc.remove ~equal:LC.equal_value kvs k))
      | _ -> assert false)
    | ( C_SET_EMPTY, []) -> return @@ V_Set ([])
    | ( C_SET_ADD , [ v ; V_Set l ] ) -> return @@ V_Set (List.dedup_and_sort ~compare:LC.compare_value (v::l))
    | ( C_FOLD , [ V_Func_val {arg_binder ; body ; env}  ; V_Set elts ; init ] )
    | ( C_SET_FOLD , [ V_Func_val {arg_binder ; body ; env}  ; V_Set elts ; init ] ) ->
      let* set_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 1 in
      let* acc_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 2 in
      let* ty = monad_option (Errors.generic_error set_ty.location "Expected set type") @@ Ast_typed.get_t_set set_ty in
      Monad.bind_fold_list
        (fun prev elt ->
          let fold_args = v_pair (prev,elt) in
          let env' = Env.extend env arg_binder (Ast_typed.(t_pair acc_ty ty), fold_args) in
          eval_ligo body calltrace env'
        )
        init elts
    | ( C_SET_FOLD_DESC , [ V_Func_val {arg_binder ; body ; env}  ; V_Set elts ; init ] ) ->
      let* set_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 1 in
      let* acc_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 2 in
      let* ty = monad_option (Errors.generic_error set_ty.location "Expected set type") @@ Ast_typed.get_t_set set_ty in
      Monad.bind_fold_right_list
        (fun prev elt ->
          let fold_args = v_pair (prev,elt) in
          let env' = Env.extend env arg_binder (Ast_typed.(t_pair acc_ty ty), fold_args) in
          eval_ligo body calltrace env'
        )
        init elts
    | ( C_SET_ITER , [ V_Func_val {arg_binder ; body ; env}  ; V_Set (elts) ] ) ->
      let* set_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 1 in
      let* ty = monad_option (Errors.generic_error set_ty.location "Expected set type") @@ Ast_typed.get_t_set set_ty in
      Monad.bind_fold_list
        (fun _ elt ->
          let env' = Env.extend env arg_binder (ty,elt) in
          eval_ligo body calltrace env'
        )
        (V_Ct C_unit) elts
    | ( C_SET_MEM    , [ v ; V_Set (elts) ] ) -> return @@ v_bool (List.mem ~equal:LC.equal_value elts v)
    | ( C_SET_REMOVE , [ v ; V_Set (elts) ] ) -> return @@ V_Set (List.filter ~f:(fun el -> not (el = v)) elts)
    | ( C_SET_UPDATE , [ v ; b ; V_Set elts ] ) ->
      if is_true b
      then return @@ V_Set (List.dedup_and_sort ~compare:LC.compare_value (v::elts))
      else return @@ V_Set (List.filter ~f:(fun el -> not (el = v)) elts)
    | ( C_ADDRESS , [ V_Ct (C_contract { address }) ] ) ->
      return (V_Ct (C_address address))
    | ( C_TRUE , [] ) -> return @@ v_bool true
    | ( C_FALSE , [] ) -> return @@ v_bool false
    (*
    >>>>>>>>
      Test operators
    >>>>>>>>
    *)
    | ( C_TEST_ORIGINATE_FROM_FILE, args) -> (
      match protocol_version, args with
      | Environment.Protocols.Edo , [ V_Ct (C_string source_file) ; V_Ct (C_string entryp) ; storage ; V_Ct ( C_mutez amt ) ] ->
        let>> (code,size) = Compile_contract_from_file (source_file,entryp,[]) in
        let>> addr = Inject_script (loc, calltrace, code, storage, amt) in
        return @@ V_Record (LMap.of_list [ (Label "0", addr) ; (Label "1", code) ; (Label "2", size) ])
      | Environment.Protocols.Hangzhou , [ V_Ct (C_string source_file) ; V_Ct (C_string entryp) ; V_List views ; storage ; V_Ct ( C_mutez amt ) ] ->
        let views = List.map
          ~f:(fun x -> trace_option ~raise (Errors.corner_case ()) @@ get_string x)
          views
        in
        let>> (code,size) = Compile_contract_from_file (source_file,entryp,views) in
        let>> addr = Inject_script (loc, calltrace, code, storage, amt) in
        return @@ V_Record (LMap.of_list [ (Label "0", addr) ; (Label "1", code) ; (Label "2", size) ])
      | _ -> fail @@ Errors.generic_error loc "Unbound primitive. Check the protocol version you are using"
    )
    | ( C_TEST_EXTERNAL_CALL_TO_ADDRESS_EXN , [ (V_Ct (C_address address)) ; V_Michelson (Ty_code (param,_,_)) ; V_Ct ( C_mutez amt ) ] ) -> (
      let contract = { address; entrypoint = None } in
      let>> err_opt = External_call (loc,calltrace,contract,param,amt) in
      match err_opt with
      | None -> return_ct C_unit
      | Some e -> fail @@ Errors.target_lang_error loc calltrace e
    )
    | ( C_TEST_EXTERNAL_CALL_TO_ADDRESS , [ (V_Ct (C_address address)) ; V_Michelson (Ty_code (param,_,_)) ; V_Ct ( C_mutez amt ) ] ) -> (
      let contract = { address; entrypoint = None } in
      let>> err_opt = External_call (loc,calltrace,contract,param,amt) in
      match err_opt with
      | None -> return (LC.v_ctor "Success" (LC.v_unit ()))
      | Some e ->
        let>> a = State_error_to_value e in
        return a
    )
    | ( C_TEST_SET_NOW , [ V_Ct (C_timestamp t) ] ) ->
      let>> () = Set_now (loc,calltrace,t) in
      return_ct C_unit
    | ( C_TEST_SET_SOURCE , [ addr ] ) ->
      let>> () = Set_source addr in
      return_ct C_unit
    | ( C_TEST_SET_BAKER , [ addr ] ) ->
      let>> () = Set_baker addr in
      return_ct C_unit
    | ( C_TEST_GET_STORAGE_OF_ADDRESS , [ addr ] ) ->
      let>> storage = Get_storage_of_address (loc, calltrace, addr) in
      return storage
    | ( C_TEST_GET_BALANCE , [ addr ] ) ->
      let>> balance = Get_balance (loc, calltrace, addr) in
      return balance
    | ( C_TEST_MICHELSON_EQUAL , [ a ; b ] ) ->
      let>> b = Michelson_equal (loc,a,b) in
      return_ct (C_bool b)
    | ( C_TEST_LOG , [ v ]) ->
      let () = Format.printf "%a\n" Ligo_interpreter.PP.pp_value v in
      return_ct C_unit
    | ( C_TEST_BOOTSTRAP_CONTRACT , [ V_Ct (C_mutez z) ; contract ; storage ] ) ->
       let* contract_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 1 in
       let* storage_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 2 in
       let>> code = Compile_contract (loc, contract, contract_ty) in
       let>> storage = Eval (loc, storage, storage_ty) in
       let>> () = Bootstrap_contract ((Z.to_int z), code, storage, contract_ty) in
       return_ct C_unit
    | ( C_TEST_NTH_BOOTSTRAP_CONTRACT , [ V_Ct (C_nat n) ] ) ->
       let n = Z.to_int n in
       let>> address = Nth_bootstrap_contract n in
       return_ct (C_address address)
    | ( C_TEST_STATE_RESET , [ n ; amts ] ) ->
      let>> () = Reset_state (loc,calltrace,n,amts) in
      return_ct C_unit
    | ( C_TEST_GET_NTH_BS , [ n ] ) ->
      let>> x = Get_bootstrap (loc,n) in
      return x
    | ( C_TEST_LAST_ORIGINATIONS , [ _ ] ) ->
      let>> x = Get_last_originations () in
      return x
    | ( C_TEST_MUTATE_VALUE , [ V_Ct (C_nat n); v ] ) -> (
      let* () = check_value v in
      let* value_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 1 in
      let v = Mutation.mutate_some_value ~raise loc n v value_ty in
      match v with
      | None ->
         return (v_none ())
      | Some (e, m) ->
         let* v = eval_ligo e calltrace env in
         return @@ (v_some (V_Record (LMap.of_list [ (Label "0", v) ; (Label "1", V_Mutation m) ]))))
    | ( C_TEST_MUTATION_TEST , [ v; tester ] ) -> (
      let* () = check_value v in
      let* value_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 0 in
      let l = Mutation.mutate_all_value ~raise loc v value_ty in
      let aux : Ast_typed.expression * 'a -> (value * 'a) option t = fun (e, m) ->
        let* v = eval_ligo e calltrace env in
        let r = match tester with
          | V_Func_val {arg_binder ; body ; env; rec_name = None ; orig_lambda } ->
            let* in_ty, _ = monad_option (Errors.generic_error loc "Expected function type") @@
                             Ast_typed.get_t_function orig_lambda.type_expression in
            let f_env' = Env.extend env arg_binder (in_ty, v) in
            eval_ligo body (loc :: calltrace) f_env'
          | V_Func_val {arg_binder ; body ; env; rec_name = Some fun_name; orig_lambda } ->
            let* in_ty, _ = monad_option (Errors.generic_error loc "Expected function type") @@
                              Ast_typed.get_t_function orig_lambda.type_expression in
            let f_env' = Env.extend env arg_binder (in_ty, v) in
            let f_env'' = Env.extend f_env' fun_name (orig_lambda.type_expression, tester) in
            eval_ligo body (loc :: calltrace) f_env''
          | _ -> raise.raise @@ Errors.generic_error loc "Trying to apply on something that is not a function?"
        in
        try_or (let* v = r in return (Some (v, m))) (return None)
      in
      let* r = iter_while aux l in
      match r with
       | None -> return (v_none ())
       | Some (v, m) -> return (v_some (V_Record (LMap.of_list [ (Label "0", v) ; (Label "1", V_Mutation m) ])))
    )
    | ( C_TEST_MUTATION_TEST_ALL , [ v; tester ] ) ->
      let* () = check_value v in
      let* value_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 0 in
      let l = Mutation.mutate_all_value ~raise loc v value_ty in
      let* mutations = bind_map_list (fun (e, m) ->
        let* v = eval_ligo e calltrace env in
        let r =  match tester with
          | V_Func_val {arg_binder ; body ; env; rec_name = None ; orig_lambda } ->
             let in_ty, _ = Ast_typed.get_t_function_exn orig_lambda.type_expression in
             let f_env' = Env.extend env arg_binder (in_ty, v) in
             eval_ligo body (loc :: calltrace) f_env'
          | V_Func_val {arg_binder ; body ; env; rec_name = Some fun_name; orig_lambda } ->
             let in_ty, _ = Ast_typed.get_t_function_exn orig_lambda.type_expression in
             let f_env' = Env.extend env arg_binder (in_ty, v) in
             let f_env'' = Env.extend f_env' fun_name (orig_lambda.type_expression, tester) in
             eval_ligo body (loc :: calltrace) f_env''
          | _ -> fail @@ Errors.generic_error loc "Trying to apply on something that is not a function?" in
        Monad.try_or (let* v = r in return (Some (v, m))) (return None)) l in
      let r = List.map ~f:(fun (v, m) -> V_Record (LMap.of_list [ (Label "0", v) ; (Label "1", V_Mutation m) ])) @@ List.filter_opt mutations in
      return (V_List r)
    | ( C_TEST_SAVE_MUTATION , [(V_Ct (C_string dir)) ; (V_Mutation ((loc, _) as mutation)) ] ) ->
      let* reg = monad_option (Errors.generic_error loc "Not a valid mutation") @@ Location.get_file loc in
      let file_contents = Fuzz.Ast_typed.buffer_of_mutation mutation in
      let id = Fuzz.Ast_typed.get_mutation_id mutation in
      let file_path = reg # file in
      (try
        let odir = Sys.getcwd () in
        let () = Sys.chdir dir in
        let file_path = Filename.basename file_path in
        let file_path = Filename.remove_extension file_path ^ "." ^ id ^ Filename.extension file_path in
        let out_chan = open_out_bin file_path in
        let () = Buffer.output_buffer out_chan file_contents in
        let () = Sys.chdir odir in
        return (v_some (v_string file_path))
       with
       | Sys_error _ ->
          return (v_none ()))
    | ( C_TEST_TO_CONTRACT , [ addr ] ) ->
       let* contract_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 0 in
       let>> code = To_contract (loc, addr, None, contract_ty) in
       return code
    | ( C_TEST_TO_ENTRYPOINT , [ V_Ct (C_string ent) ; addr ] ) ->
       let* contract_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 0 in
       let>> code = To_contract (loc, addr, Some ent, contract_ty) in
       return code
    | ( C_TEST_TO_TYPED_ADDRESS , [ V_Ct (C_contract { address; _ }) ] ) ->
       let>> () = Check_storage_address (loc, address, expr_ty) in
       let addr = LT.V_Ct ( C_address address ) in
       return addr
    | ( C_TEST_RUN , [ V_Func_val f ; v ] ) ->
       let* () = check_value (V_Func_val f) in
       let* () = check_value v in
       let>> code = Run (loc, f, v) in
       return code
    | ( C_TEST_EVAL , [ v ] )
    | ( C_TEST_COMPILE_META_VALUE , [ v ] ) ->
       let* () = check_value v in
       let* value_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 0 in
       let>> code = Eval (loc, v, value_ty) in
       return code
    | ( C_TEST_GET_STORAGE , [ addr ] ) ->
       let* typed_address_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 0 in
       let storage_ty = match Ast_typed.get_t_typed_address typed_address_ty with
         | Some (_, storage_ty) -> storage_ty
         | _ -> failwith "Expecting typed_address" in
       let>> value = Get_storage(loc, calltrace, addr, storage_ty) in
       return value
    | ( C_TEST_ORIGINATE , [ contract ; storage ; V_Ct ( C_mutez amt ) ] ) ->
       let* contract_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 0 in
       let* storage_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 1 in
       let>> code = Compile_contract (loc, contract, contract_ty) in
       let>> storage = Eval (loc, storage, storage_ty) in
       let>> size = Get_size code in
       let>> addr  = Inject_script (loc, calltrace, code, storage, amt) in
       return @@ V_Record (LMap.of_list [ (Label "0", addr) ; (Label "1", code) ; (Label "2", size) ])
    | ( C_TEST_EXTERNAL_CALL_TO_CONTRACT_EXN , [ (V_Ct (C_contract contract)) ; param ; V_Ct ( C_mutez amt ) ] ) ->
       let* param_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 1 in
       let>> param = Eval (loc, param, param_ty) in
       (match param with
       | V_Michelson (Ty_code (param,_,_)) ->
          let>> err_opt = External_call (loc,calltrace,contract,param,amt) in
          (match err_opt with
                     | None -> return @@ V_Ct C_unit
                     | Some e -> fail @@ Errors.target_lang_error loc calltrace e)
       | _ -> fail @@ Errors.generic_error loc "Error typing param")
    | ( C_TEST_EXTERNAL_CALL_TO_CONTRACT , [ (V_Ct (C_contract contract)) ; param; V_Ct ( C_mutez amt ) ] ) ->
       let* param_ty = monad_option (Errors.generic_error loc "Could not recover types") @@ List.nth types 1 in
       let>> param = Eval (loc, param, param_ty) in
       (match param with
       | V_Michelson (Ty_code (param,_,_)) ->
          let>> err_opt = External_call (loc,calltrace,contract,param,amt) in
          (match err_opt with
           | None -> return (LC.v_ctor "Success" (LC.v_unit ()))
           | Some e ->
              let>> a = State_error_to_value e in
              return a)
       | _ -> fail @@ Errors.generic_error loc "Error typing param")
    | ( C_TEST_NTH_BOOTSTRAP_TYPED_ADDRESS , [ V_Ct (C_nat n) ] ) ->
      let n = Z.to_int n in
      let* parameter_ty', storage_ty' = monad_option (Errors.generic_error loc "Expected typed address") @@
                                          Ast_typed.get_t_typed_address expr_ty in
      let>> (address, parameter_ty, storage_ty) = Nth_bootstrap_typed_address (loc, n) in
      let* () = monad_option (Errors.generic_error loc "Parameter in bootstrap contract does not match") @@
                   Ast_typed.assert_type_expression_eq (parameter_ty, parameter_ty') in
      let* () = monad_option (Errors.generic_error loc "Storage in bootstrap contract does not match") @@
                   Ast_typed.assert_type_expression_eq (storage_ty, storage_ty') in
      return_ct (C_address address)
    | ( C_TEST_SET_BIG_MAP , [ V_Ct (C_int n) ; V_Map kv ] ) ->
      let bigmap_ty = List.nth_exn types 1 in
      let>> () = Set_big_map (n, kv, bigmap_ty) in
      return_ct (C_unit)
    | ( C_TEST_CAST_ADDRESS , [ V_Ct (C_address x) ] ) ->
      return_ct (C_address x)
    | ( C_TEST_CREATE_CHEST , [ V_Ct (C_bytes payload) ; V_Ct (C_nat time)] ) ->
      let (chest,chest_key) = Michelson_backend.create_chest payload (Z.to_int time) in
      return @@ v_pair (V_Ct (C_bytes chest) , V_Ct (C_bytes chest_key))
    | ( C_TEST_CREATE_CHEST_KEY , [ V_Ct (C_bytes chest) ; V_Ct (C_nat time)] ) ->
      let chest_key = Michelson_backend.create_chest_key chest (Z.to_int time) in
      return @@ V_Ct (C_bytes chest_key)
    | ( C_FAILWITH , [ a ] ) ->
      fail @@ Errors.meta_lang_failwith loc calltrace a
    | _ -> fail @@ Errors.generic_error loc "Unbound primitive."
  )

(*interpreter*)
and eval_literal : Ast_typed.literal -> value Monad.t = function
  | Literal_unit        -> Monad.return @@ V_Ct (C_unit)
  | Literal_int i       -> Monad.return @@ V_Ct (C_int i)
  | Literal_nat n       -> Monad.return @@ V_Ct (C_nat n)
  | Literal_timestamp i -> Monad.return @@ V_Ct (C_timestamp i)
  | Literal_string s    -> Monad.return @@ V_Ct (C_string (Ligo_string.extract s))
  | Literal_bytes s     -> Monad.return @@ V_Ct (C_bytes s)
  | Literal_mutez s     -> Monad.return @@ V_Ct (C_mutez s)
  | Literal_key_hash s  ->
     begin
       match Tezos_crypto.Signature.Public_key_hash.of_b58check s with
       | Ok kh -> Monad.return @@ V_Ct (C_key_hash kh)
       | Error _ -> Monad.fail @@ Errors.literal Location.generated (Literal_key_hash s)
     end
  | Literal_address s   ->
     begin
       match Tezos_protocol_011_PtHangzH.Protocol.Alpha_context.Contract.of_b58check s with
       | Ok t -> Monad.return @@ V_Ct (C_address t)
       | Error _ -> Monad.fail @@ Errors.literal Location.generated (Literal_address s)
     end
  | l -> Monad.fail @@ Errors.literal Location.generated l

and eval_ligo ~raise ~steps ~protocol_version : Ast_typed.expression -> calltrace -> env -> value Monad.t
  = fun term calltrace env ->
    let eval_ligo ?(steps = steps - 1) = eval_ligo ~raise ~steps ~protocol_version in
    let open Monad in
    let* () = if steps <= 0 then fail (Errors.meta_lang_eval term.location calltrace "Out of fuel") else return () in
    match term.expression_content with
    | E_type_inst _ ->
       fail @@ Errors.generic_error term.location "Polymorphism not supported: polymorphic expressions should be monomorphized before being interpreted. This could mean that the expression that you are trying to interpret is too generic, try adding a type annotation."
    | E_application {lamb = f; args} -> (
        let* f' = eval_ligo f calltrace env in
        let* args' = eval_ligo args calltrace env in
        match f' with
          | V_Func_val {arg_binder ; body ; env; rec_name = None ; orig_lambda } ->
            let in_ty, _ = Ast_typed.get_t_function_exn orig_lambda.type_expression in
            let f_env' = Env.extend env arg_binder (in_ty, args') in
            eval_ligo body (term.location :: calltrace) f_env'
          | V_Func_val {arg_binder ; body ; env; rec_name = Some fun_name; orig_lambda} ->
            let in_ty, _ = Ast_typed.get_t_function_exn orig_lambda.type_expression in
            let f_env' = Env.extend env arg_binder (in_ty, args') in
            let f_env'' = Env.extend f_env' fun_name (orig_lambda.type_expression, f') in
            eval_ligo body (term.location :: calltrace) f_env''
          | V_Ligo (_, code) ->
            let>> ctxt = Get_state () in
            return @@ Michelson_backend.run_michelson_code ~raise ~loc:term.location ctxt code term.type_expression args' args.type_expression
          | _ -> fail @@ Errors.generic_error term.location "Trying to apply on something that is not a function?"
      )
    | E_lambda {binder; result;} ->
      return @@ V_Func_val {rec_name = None; orig_lambda = term ; arg_binder=binder ; body=result ; env}
    | E_let_in {let_binder ; rhs; let_result; attr = { no_mutation }} -> (
      let* rhs' = eval_ligo rhs calltrace env in
      eval_ligo (let_result) calltrace (Env.extend env let_binder ~no_mutation (rhs.type_expression,rhs'))
    )
    | E_type_in {type_binder=_ ; rhs=_; let_result} -> (
      eval_ligo (let_result) calltrace env
    )
    | E_mod_in {module_binder; rhs; let_result} ->
       let>> state = Get_state () in
       let (item, state) = eval_module ~raise ~steps ~protocol_version (rhs, state, env) in
       let>> () = Put_state state in
       let env = Env.extend_mod env module_binder item in
       eval_ligo (let_result) calltrace env
    | E_mod_alias {alias;binders;result} ->
       let module_env = resolve_module_path ~raise ~loc:term.location binders env in
       eval_ligo (result) calltrace (Env.extend_mod env alias module_env)
    | E_literal l ->
      eval_literal l
    | E_variable var ->
      let {eval_term=v} = try fst (Option.value_exn (Env.lookup env var)) with _ -> (failwith (Format.asprintf "unbound variable: %a" Ast_typed.PP.expression_variable var)) in
      return v
    | E_record recmap ->
      let* lv' = Monad.bind_map_list
        (fun (label,(v:Ast_typed.expression)) ->
          let* v' = eval_ligo v calltrace env in
          return (label,v'))
        (LMap.to_kv_list_rev recmap)
      in
      return @@ V_Record (LMap.of_list lv')
    | E_record_accessor { record ; path} -> (
      let* record' = eval_ligo record calltrace env in
      match record' with
      | V_Record recmap ->
        let a = LMap.find path recmap in
        return a
      | _ -> failwith "trying to access a non-record"
    )
    | E_record_update {record ; path ; update} -> (
      let* record' = eval_ligo record calltrace env in
      match record' with
      | V_Record recmap ->
        if LMap.mem path recmap then
          let* field' = eval_ligo update calltrace env in
          return @@ V_Record (LMap.add path field' recmap)
        else
          failwith "field l does not exist in record"
      | _ -> failwith "this expression isn't a record"
    )
    | E_constant {cons_name ; arguments} -> (
      let* arguments' = Monad.bind_map_list
        (fun (ae:Ast_typed.expression) ->
          let* value = eval_ligo ae calltrace env in
          return @@ (value, ae.type_expression))
        arguments in
      apply_operator ~raise ~steps ~protocol_version term.location calltrace term.type_expression env cons_name arguments'
    )
    | E_constructor { constructor = Label c ; element } when String.equal c "True"
      && element.expression_content = Ast_typed.e_unit () -> return @@ V_Ct (C_bool true)
    | E_constructor { constructor = Label c ; element } when String.equal c "False"
      && element.expression_content = Ast_typed.e_unit () -> return @@ V_Ct (C_bool false)
    | E_constructor { constructor = Label c ; element } ->
       let* v' = eval_ligo element calltrace env in
      return @@ V_Construct (c,v')
    | E_matching { matchee ; cases} -> (
      let* e' = eval_ligo matchee calltrace env in
      match cases, e' with
      | Match_variant {cases;_}, V_List [] ->
        let {constructor=_ ; pattern=_ ; body} =
          List.find_exn
            ~f:(fun {constructor = (Label c) ; pattern=_ ; body=_} ->
              String.equal "Nil" c)
            cases in
        eval_ligo body calltrace env
      | Match_variant {cases;tv}, V_List lst ->
        let {constructor=_ ; pattern ; body} =
          List.find_exn
            ~f:(fun {constructor = (Label c) ; pattern=_ ; body=_} ->
              String.equal "Cons" c)
            cases in
        let ty = Ast_typed.get_t_list_exn tv in
        let hd = List.hd_exn lst in
        let tl = V_List (List.tl_exn lst) in
        let proj = v_pair (hd,tl) in
        let env' = Env.extend env pattern (ty, proj) in
        eval_ligo body calltrace env'
      | Match_variant {cases;_}, V_Ct (C_bool b) ->
        let ctor_body (case : matching_content_case) = (case.constructor, case.body) in
        let cases = LMap.of_list (List.map ~f:ctor_body cases) in
        let get_case c =
            (LMap.find (Label c) cases) in
        let match_true  = get_case "True" in
        let match_false = get_case "False" in
        if b then eval_ligo match_true calltrace env
        else eval_ligo match_false calltrace env
      | Match_variant {cases ; tv} , V_Construct (matched_c , proj) ->
        let* tv = match Ast_typed.get_t_sum tv with
          | Some tv ->
             let {associated_type} = LMap.find
                                  (Label matched_c) tv.content in
             return associated_type
          | None ->
             match Ast_typed.get_t_option tv with
             | Some tv -> return tv
             | None ->
                fail @@
                  (Errors.generic_error tv.location "Expected sum") in
        let {constructor=_ ; pattern ; body} =
          List.find_exn
            ~f:(fun {constructor = (Label c) ; pattern=_ ; body=_} ->
              String.equal matched_c c)
            cases in
        (* TODO-er: check *)
        let env' = Env.extend env pattern (tv, proj) in
        eval_ligo body calltrace env'
      | Match_record {fields ; body ; tv = _} , V_Record rv ->
        let aux : label -> ( expression_variable * _ ) -> env -> env =
          fun l (v,ty) env ->
            let iv = match LMap.find_opt l rv with
              | Some x -> x
              | None -> failwith "label do not match"
            in
            Env.extend env v (ty,iv)
        in
        let env' = LMap.fold aux fields env in
        eval_ligo body calltrace env'
      | _ , v -> failwith ("not yet supported case "^ Format.asprintf "%a" Ligo_interpreter.PP.pp_value v^ Format.asprintf "%a" Ast_typed.PP.expression term)
    )
    | E_recursive {fun_name; fun_type=_; lambda} ->
      return @@ V_Func_val { rec_name = Some fun_name ;
                             orig_lambda = term ;
                             arg_binder = lambda.binder ;
                             body = lambda.result ;
                             env = env }
    | E_raw_code {language ; code} -> (
      let open Ast_typed in
      match code.expression_content with
      | E_literal (Literal_string _) when String.equal language Stage_common.Backends.michelson &&
                                           is_t_function (get_type_expression code) ->
        let in_type, out_type = trace_option ~raise (Errors.generic_error term.location "Expected function") @@
                                   get_t_function (get_type_expression code) in
        let arg_binder = Location.wrap @@ Var.fresh () in
        let body = e_a_application term (e_a_variable arg_binder in_type) out_type in
        let orig_lambda = e_a_lambda { binder = arg_binder ; result = body } in_type out_type in
        return @@ V_Func_val { rec_name = None ; orig_lambda ; body ; env ; arg_binder }
      | E_literal (Literal_string x) ->
        let exp_as_string = Ligo_string.extract x in
        return @@ V_Ligo (language , exp_as_string)
      | _ -> failwith "impossible"
    )
    | E_module_accessor _ ->
       let rec aux env e = match e.expression_content with
         | E_variable var ->
            let* value_expr, _ = monad_option (Errors.generic_error term.location "Error resolving module path: variable not found")
                               (List.Assoc.find (Ligo_interpreter.Environment.expressions env) ~equal:(Location.equal_content ~equal:Var.equal) var) in
            return value_expr.eval_term
         | E_record_accessor {record = { expression_content = E_variable record }; path} ->
            let* value_rcrd, _ = monad_option (Errors.generic_error term.location "Error resolving module path: record not found")
                               (List.Assoc.find (Ligo_interpreter.Environment.expressions env) ~equal:(Location.equal_content ~equal:Var.equal) record) in
            (match value_rcrd.eval_term with
             | V_Record recmap ->
                let a = LMap.find path recmap in
                return a
             | _ -> raise.raise @@ Errors.generic_error term.location @@ "Error resolving module path")
         | E_module_accessor {module_name;element} ->
            let module_env =  match List.Assoc.find (Ligo_interpreter.Environment.modules env) ~equal:String.equal module_name with
              | None -> raise.raise @@ Errors.generic_error term.location @@ "Error resolving module path"
              | Some e -> e in
            aux module_env element
         | _ -> raise.raise @@ Errors.generic_error term.location "Unsupported module path"
       in aux env term

and try_eval ~raise ~steps ~protocol_version expr env state r = Monad.eval ~raise (eval_ligo ~raise ~steps ~protocol_version expr [] env) state r

and resolve_module_path ~raise ~loc binders env =
  let aux (e : env) (m : module_variable) =
    match List.Assoc.find (Ligo_interpreter.Environment.modules e) ~equal:String.equal m with
    | None -> raise.raise @@ Errors.generic_error loc "Error resolving module path"
    | Some e -> e in
  List.Ne.fold_left aux env binders

and eval_module ~raise ~steps ~protocol_version : Ast_typed.module_fully_typed * Tezos_state.context * env -> env * Tezos_state.context =
  fun (Module_Fully_Typed prg, initial_state, env) ->
    let aux : env * env * Tezos_state.context -> declaration location_wrap -> env * env * Tezos_state.context =
      fun (top_env,curr_env,state) el ->
        match Location.unwrap el with
        | Ast_typed.Declaration_type _ ->
           (top_env,curr_env,state)
        | Ast_typed.Declaration_constant {binder; expr ; attr = { inline=_ ; no_mutation }} ->
          let (v,state) = try_eval ~raise ~steps ~protocol_version expr top_env state None in
          let curr_env' = Env.extend curr_env binder ~no_mutation (expr.type_expression, v) in
          let top_env' = Env.extend top_env binder ~no_mutation (expr.type_expression, v) in
          (top_env', curr_env',state)
        | Ast_typed.Declaration_module {module_binder; module_} ->
          let (inner_curr_env, state) = eval_module ~raise ~steps ~protocol_version (module_, state, top_env) in
          let curr_env' = Env.extend_mod curr_env module_binder inner_curr_env in
          let top_env' = Env.extend_mod top_env module_binder inner_curr_env in
          (top_env', curr_env',state)
        | Ast_typed.Module_alias {alias;binders} ->
          let module_env = resolve_module_path ~raise ~loc:el.location binders top_env in
          let top_env' = Env.extend_mod top_env alias module_env in
          let curr_env' = Env.extend_mod curr_env alias module_env in
          (top_env', curr_env',state)
    in
    let (_, curr_env, state) = List.fold ~f:aux ~init:(env,[], initial_state) prg in
    (curr_env, state)

let eval_test ~raise ~steps ~protocol_version : Ast_typed.module_fully_typed -> (env * (string * value) list) =
  fun prg ->
    let initial_state = Tezos_state.init_ctxt ~raise protocol_version [] in
    let (env, _state) = eval_module ~raise ~steps ~protocol_version (prg, initial_state, Env.empty_env) in
    let v = Env.to_kv_list_rev (Ligo_interpreter.Environment.expressions env) in
    let aux : expression_variable * (value_expr * bool) -> (string * value) option = fun (ev, (v, _)) ->
      let ev = Location.unwrap ev in
      if not (Var.is_generated ev) && (Base.String.is_prefix (Var.to_name ev) ~prefix:"test") then
        Some (Var.to_name ev, v.eval_term)
      else
        None
    in
    (* NOTE: The environment is returned so that it can be used in tests *)
    (env , List.filter_map ~f:aux v)

let () = Printexc.record_backtrace true
