open Ast_core
(* open Typesystem.Solver_types *)
open Format
module UF = UnionFind.Poly2

type 'a pretty_printer = Format.formatter -> 'a -> unit

let type_constraint_ : _ -> type_constraint_simpl -> unit = fun ppf ->
  function
  |SC_Apply _ -> fprintf ppf "Apply"
  |SC_Abs _ -> fprintf ppf "Abs"
  |SC_Constructor { tv; c_tag; tv_list=_ } ->
    let ct = match c_tag with
      | C_arrow        -> "arrow"
      | C_option       -> "option"
      | C_map          -> "map"
      | C_big_map      -> "big_map"
      | C_list         -> "list"
      | C_set          -> "set"
      | C_unit         -> "unit"
      | C_string       -> "string"
      | C_nat          -> "nat"
      | C_mutez        -> "mutez"
      | C_timestamp    -> "timestamp"
      | C_int          -> "int"
      | C_address      -> "address"
      | C_bytes        -> "bytes"
      | C_key_hash     -> "key_hash"
      | C_key          -> "key"
      | C_signature    -> "signature"
      | C_operation    -> "operation"
      | C_contract     -> "contract"
      | C_chain_id     -> "chain_id"
      | C_bls12_381_g1 -> "bls12_381_g1"
      | C_bls12_381_g2 -> "bls12_381_g2"
      | C_bls12_381_fr -> "bls12_381_fr"
      | C_never        -> "never"
    in
    fprintf ppf "CTOR %a %s()" Var.pp tv ct
  |SC_Alias       { a; b } -> fprintf ppf "Alias %a %a" Var.pp a Var.pp b
  |SC_Poly        _ -> fprintf ppf "Poly"
  |SC_Typeclass   _ -> fprintf ppf "TC"
  |SC_Access_label _ -> fprintf ppf "Access_label"
  |SC_Row { tv; r_tag; tv_map=_ } ->
    let r = match r_tag with
      | C_record       -> "record"
      | C_variant      -> "variant"
    in
    fprintf ppf "ROW %a %s()" Var.pp tv r

let type_constraint : _ -> type_constraint_simpl -> unit = fun ppf c ->
  fprintf ppf "%a (reason: %s)" type_constraint_ c (reason_simpl c)

let all_constraints ppf ac =
  fprintf ppf "[%a]" (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ";\n") type_constraint) ac

let aliases ppf (_al : unionfind) =
  fprintf ppf "ALIASES [DEBUG:temporarily disabled to debug union-find]" (* fprintf ppf "ALIASES %a" UF.print al *)

(* let structured_dbs : _ -> structured_dbs -> unit = fun ppf structured_dbs ->
 *   let { all_constraints = a ; aliases = b ; _ } = structured_dbs in
 *   fprintf ppf "STRUCTURED_DBS\n %a\n %a" all_constraints a aliases b *)

(* let already_selected_and_propagators : _ -> _ ex_propagator_state list -> unit = fun ppf already_selected ->
 *   let _ = already_selected in
 *   fprintf ppf "ALREADY_SELECTED" *)

(* let state : _ -> _ typer_state -> unit = fun ppf state ->
 *   let { structured_dbs=a ; already_selected_and_propagators =b } = state in
 *   fprintf ppf "STATE %a %a" structured_dbs a already_selected_and_propagators b *)
