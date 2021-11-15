open Tezos_utils
open Michelson
open Tezos_micheline.Micheline

type ('l, 'error) mapper = 'l michelson -> 'l michelson

let rec map_expression : ('l, 'error) mapper -> 'l michelson -> 'l michelson = fun f e ->
  let self = map_expression f in
  let e' = f e in
  match e' with
  | Prim (l , p , lst , a) -> (
      let lst' = List.map ~f:self lst in
      Prim (l , p , lst' , a)
    )
  | Seq (l , lst) -> (
      let lst' = List.map ~f:self lst in
      Seq (l , lst')
    )
  | x -> x

let fetch_contract_ty_inputs : _ michelson -> (_ michelson * _ michelson) option =
  function
  | Prim (_, "lambda", [Prim (_, "pair", [param_ty; storage_ty], _); _], _) ->
    Some (param_ty, storage_ty)
  | _ -> None

let fetch_views_ty : _ michelson -> (_ michelson * _ michelson) option =
  function
  | Prim (_, "lambda", [Prim (_, "pair", [param_ty; _storage_ty], _); ret_ty], _) ->
    Some (param_ty, ret_ty)
  | _ -> None