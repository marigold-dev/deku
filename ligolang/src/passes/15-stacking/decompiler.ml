open Errors
open Mini_c.Types
open Tezos_micheline.Micheline
open Trace

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

let rec decompile_value ~raise :
  ('l, string) node -> ('l, string) node -> value =
  fun ty value ->
  let ty = normalize_edo_comb_type ty in
  let value = normalize_edo_comb_value ty value in
  match (ty, value) with
  | Prim (_, "pair", ts, _), Prim (_, "Pair", vs, _) -> (
      let els = List.map ~f:(fun (t,v) -> decompile_value ~raise t v) (List.zip_exn ts vs) in
      let rec aux l : value =
        match l with
        | [] -> raise.raise (untranspilable ty value)
        | [x] -> x
        | hd::tl -> (
            let tl' = aux tl in
            D_pair (hd, tl')
          ) in
      aux els
    )
  | Prim (_, "or", [a_ty; _], _), Prim (_, "Left", [a], _) -> (
      let a = decompile_value ~raise a_ty a in
      D_left a
    )
  | Prim (_, "or", [_; b_ty], _), Prim (_, "Right", [b], _) -> (
      let b = decompile_value ~raise b_ty b in
      D_right b
    )
  | Prim (_, "int", [], _), Int (_, n) ->
      D_int n
  | Prim (_, "nat", [], _), Int (_, n) ->
      D_nat n
  | Prim (_, "chain_id", _, _), String (_, id) ->
    (* Before EDO :
      let id = Tezos_base.TzPervasives.Chain_id.of_bytes_exn id in
      let str = Tezos_crypto.Base58.simple_encode
      (Tezos_base__TzPervasives.Chain_id.b58check_encoding)
      id in
    *)
    D_string id
  | Prim (_, "key_hash", [], _), String (_, n) ->
    D_string n
  | Prim (_, "key", [], _), String (_, n) ->
    D_string n
  | Prim (_, "signature", [], _), String (_, n) ->
    D_string n
  | Prim (_, "timestamp", [], _), Int (_, n) ->
      D_timestamp n
  | Prim (_, "timestamp", [], _), String (_, n) ->
      let open Tezos_base.TzPervasives.Time.Protocol in
      let n = Z.of_int64 (to_seconds (of_notation_exn n)) in
      D_timestamp n
  | Prim (_, "mutez", [], _), Int (_, n) ->
      D_mutez n
  | Prim (_, "bool", [], _), Prim (_, "True", [], _) ->
      D_bool true
  | Prim (_, "bool", [], _), Prim (_, "False", [], _) ->
      D_bool false
  | Prim (_, "string", [], _), String (_, s) ->
      D_string s
  | Prim (_, "bytes", [], _), Bytes (_, b) ->
      D_bytes b
  | Prim (_, "address", [], _), String (_, s) ->
      D_string s
  | Prim (_, "unit", [], _), Prim (_, "Unit", [], _) ->
      D_unit
  | Prim (_, "option", [_], _), Prim (_, "None", [], _) ->
      D_none
  | Prim (_, "option", [o_ty], _), Prim (_, "Some", [s], _) ->
      let s' = decompile_value ~raise o_ty s in
      D_some s'
  | Prim (_, "map", [k_ty; v_ty], _), Seq (_, lst) ->
      let lst' =
        let aux elt =
          match elt with
          | Prim (_, "Elt", [k; v], _) ->
            let k' = decompile_value ~raise k_ty k in
            let v' = decompile_value ~raise v_ty v in
            (k', v')
          | _ ->
            let ty = root (strip_locations ty) in
            let value = root (strip_locations value) in
            raise.raise (untranspilable ty value)
        in
        List.map ~f:aux lst
      in
      D_map lst'
  | Prim (_, "big_map", [k_ty; v_ty], _), Seq (_, lst) ->
      let lst' =
        let aux elt =
          match elt with
          | Prim (_, "Elt", [k; v], _) ->
            let k' = decompile_value ~raise k_ty k in
            let v' = decompile_value ~raise v_ty v in
            (k', v')
          | _ ->
            let ty = root (strip_locations ty) in
            let value = root (strip_locations value) in
            raise.raise (untranspilable ty value)
        in
        List.map ~f:aux lst
      in
      D_big_map lst'
  | Prim (_, "big_map", [_; _], _), Int (_, v) ->
      D_nat v
  | Prim (_, "list", [ty], _), Seq (_, lst) ->
      let lst' =
        List.map ~f:(decompile_value ~raise ty) lst
      in
      D_list lst'
  | Prim (_, "set", [ty], _), Seq (_, lst) -> (
      let lst' =
        let aux acc cur = cur :: acc in
        let lst = List.fold_left ~f:aux ~init:lst [] in
        List.rev lst in
      let lst'' =
        let aux = fun t -> decompile_value ~raise ty t in
        List.map ~f:aux lst'
      in
      D_set lst''
    )
  | Prim (_, "operation", [], _), Bytes (_, op) -> (
      D_operation op
    )
  | Prim (_, "lambda", [_; _], _) as ty, Seq (_, _) ->
      let pp_lambda =
        Format.asprintf "[lambda of type: %a ]" Michelson.pp ty in
        D_string pp_lambda
  | Prim (xx, "ticket", [ty], _) , Prim (_, "Pair", [addr;v;amt], _) ->
    ignore addr;
    let ty_nat = Prim (xx, "nat", [], []) in
    let v' = decompile_value ~raise ty v in
    let amt' = decompile_value ~raise ty_nat amt in
    D_ticket (v', amt')
 | ty, v ->
      raise.raise (untranspilable ty v)
