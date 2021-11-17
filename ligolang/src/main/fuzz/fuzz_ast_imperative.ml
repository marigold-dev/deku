open Helpers_ast_imperative
open Ast_imperative
include Fuzz_shared.Monad

(* Helpers for swapping operators *)

let binary_num_constants = [C_MUL; C_DIV; C_MOD; C_SUB; C_ADD]
let binary_bool_constants = [C_AND; C_OR; C_XOR]
let cmp_constants = [C_EQ; C_NEQ; C_LT; C_GT; C_LE; C_GE]
let op_class = [binary_num_constants; binary_bool_constants; cmp_constants]

let rec find_class op = function
  | [] -> raise Not_found
  | x :: _ when List.mem x op ~equal:Caml.(=) -> x
  | _ :: xs -> find_class op xs

(* Helpers for transforming literals *)

let transform_int =
  let const0 _ = 0 in
  let id n = n in
  let negative n = -n in
  let incr n = n + 1 in
  let pred n = n - 1 in
  let prod n = 2 * n in
  [id; const0; negative; incr; pred; prod]

let transform_nat =
  let const0 _ = 0 in
  let id n = n in
  let incr n = n + 1 in
  let prod n = 2 * n in
  [id; const0; incr; prod]

let transform_string =
  let constn _ = "" in
  let double s = s ^ s in
  let id s = s in
  [id; String.capitalize_ascii; String.uncapitalize_ascii; String.lowercase_ascii; String.uppercase_ascii; constn; double]

module Mutator (M : Monad) = struct
  open Monad_context(M)
  open Fold_helpers(M)

  let mutate_literal = function
    | Literal_int z ->
       let* z = mutate_int (Z.to_int z) in
       let* t = oneof (List.map ~f:return transform_int) in
       return (Literal_int (Z.of_int (t z)))
    | Literal_nat z ->
       let* n = mutate_nat (Z.to_int z) in
       let* t = oneof (List.map ~f:return transform_nat) in
       return (Literal_nat (Z.of_int (t n)))
    | Literal_mutez z ->
       let* n = mutate_nat (Z.to_int z) in
       let* t = oneof (List.map ~f:return transform_nat) in
       return (Literal_mutez (Z.of_int (t n)))
    | Literal_string (Standard s) ->
       let* s = mutate_string s in
       let* t = oneof (List.map ~f:return transform_string) in
       return (Literal_string (Standard (t s)))
    | Literal_string (Verbatim s) ->
       let* s = mutate_string s in
       let* t = oneof (List.map ~f:return transform_string) in
       return (Literal_string (Verbatim (t s)))
    | l ->
       return l

  let mutate_constant ({cons_name} as const) =
    match cons_name with
    | Const c when List.exists ~f:(fun l -> List.mem l c ~equal:Caml.(=)) op_class  ->
       let ops = find_class c op_class in
       let mapper x = return { const with cons_name = Const x } in
       oneof @@ List.map ~f:mapper ops
    | _ ->
       return const

  let mutate_expression (expr : expression) =
    match expr.expression_content with
    | E_literal l ->
       let* l = mutate_literal l in
       return { expr with expression_content = E_literal l }
    | E_constant c ->
       let* c = mutate_constant c in
       return { expr with expression_content = E_constant c }
    | _ ->
       return expr

  let mutate_module_ ?n (mod_ : module_) =
    let rndmod_ = map_module (Expression mutate_expression) mod_ in
    get_one ?n rndmod_

end
