open Helpers
open Cst.Cameligo
include Fuzz_shared.Monad

(* Helpers for swapping operators *)

let arith_bin_op_ctor =
  let add op = Add op in
  let sub op = Sub op in
  let mult op = Mult op in
  let div op = Div op in
  let mod_ op = Mod op in
  [add;sub;mult;div;mod_]

let bool_bin_op_ctor =
  let and_ op = And op in
  let or_ op = Or op in
  [and_;or_]

let comp_bin_op_ctor =
  let lt op = Lt op in
  let leq op = Leq op in
  let gt op = Gt op in
  let geq op = Geq op in
  let equal op = Equal op in
  let neq op = Neq op in
  [lt; leq; gt; geq; equal; neq]

module Mutator (M : Monad) = struct
  open Monad_context(M)
  open Fold_helpers(M)
  open Fuzz_shared.Helpers

  let mutate_expression (expr : expr) =
    let map_return = List.map ~f:return in
    let false_return x = return (false, x) in
    let return x = return (true, x) in
    match expr with
    | EAnnot {value = {inside = (_, _, TVar {value = "address"})}} ->
       false_return expr
    | EArith (Add op) | EArith (Sub op) | EArith (Mult op)
    | EArith (Div op) | EArith (Mod op) ->
       let* ctor = arith_bin_op_ctor |> map_return |> oneof in
       return (EArith (ctor op))
    | EArith (Int {value=(s, z);region}) ->
       let* z = mutate_int (Z.to_int z) in
       let* f = transform_int |> map_return |> oneof in
       let z = f z in
       return (EArith (Int {value=(s, Z.of_int z);region}))
    | EArith (Nat {value=(s, z);region}) ->
       let* z = mutate_nat (Z.to_int z) in
       let* f = transform_nat |> map_return |> oneof in
       let z = f z in
       return (EArith (Nat {value=(s, Z.of_int z);region}))
    | EArith (Mutez {value=(s, z);region}) ->
       let* z = mutate_nat (Z.to_int z) in
       let* f = transform_nat |> map_return |> oneof in
       let z = f z in
       return (EArith (Mutez {value=(s, Z.of_int z);region}))
    | ELogic (BoolExpr (Or op)) | ELogic (BoolExpr (And op)) ->
       let* ctor = bool_bin_op_ctor |> map_return |> oneof in
       return (ELogic (BoolExpr (ctor op)))
    | ELogic (CompExpr (Lt op)) | ELogic (CompExpr (Leq op))
    | ELogic (CompExpr (Gt op)) | ELogic (CompExpr (Geq op))
    | ELogic (CompExpr (Equal op)) | ELogic (CompExpr (Neq op)) ->
       let* ctor = comp_bin_op_ctor |> map_return |> oneof in
       return (ELogic (CompExpr (ctor op)))
    | EString (String {value=s;region}) ->
       let* s = mutate_string s in
       let* f = oneof (map_return transform_string) in
       let s = f s in
       return (EString (String {value=s;region}))
    | _ ->
       return expr

  let mutate_mapper : mapper = { e = mutate_expression;
                                 t = (fun x -> return x);
                                 d = (fun x -> return x); }

  let mutate_expression_list ?n (expr : Cst.Cameligo.expr) =
    get_list ?n @@ map_expression mutate_mapper expr

  let mutate_expression ?n (expr: Cst.Cameligo.expr) =
    let rndexpr = map_expression mutate_mapper expr in
    get_one ?n rndexpr

  let mutate_module_ ?n (mod_ : Cst.Cameligo.t) =
    let rndmod_ = map_module mutate_mapper mod_ in
    get_one ?n rndmod_

end
