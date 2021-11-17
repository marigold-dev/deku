open Ast_core.Types
open Solver_types
open Ast_core.Combinators

open Db_index_tests_common

module Cycle_detection_topological_sort_tests = struct
  include Test_vars
  module Plugin_under_test = Database_plugins.All_plugins.Cycle_detection_topological_Sort
  include  Plugin_under_test
  let repr : type_variable -> type_variable = fun tv ->
    match tv with
    | tv when Var.equal tv tva -> tva
    | tv when Var.equal tv tvb -> tva
    | _ -> tv
  let same_state ~raise:_ sa sb =
    (* This index is not implemented yet, its state is just a unit value *)
    let () = get_state_for_tests sa in
    let () = get_state_for_tests sb in
    ()
end

let cycle_detection_topological_sort ~raise () =
  let open Cycle_detection_topological_sort_tests in
  (* create empty state *)
  let state = create_state ~cmp:Ast_core.Compare.type_variable in
  (* assert state = () *)
  let () = tst_assert ~raise "state = ()" @@ (match get_state_for_tests state with () -> true) in

  (* add (tva, SC_Constructor ctor_a) to the state *)
  let ctor_a = make_c_constructor_simpl 1 None tva C_unit [] in
  let state' = add_constraint repr state (SC_Constructor ctor_a) in                                           
  (* assert state' = () because this index is not implemented yet, and its state is just a unit value *)
  let () = tst_assert ~raise "state' = ()" @@ (match get_state_for_tests state' with () -> true) in

  (* add (tvb, SC_Constructor ctor_b) to the state (tvb being an alias of tva, see repr) *)
  let ctor_b = make_c_constructor_simpl 2 None tvb C_unit [] in
  let state'' = add_constraint repr state' (SC_Constructor ctor_b) in
  (* assert state'' = () because this index is not implemented yet, and its state is just a unit value *)
  let () = tst_assert ~raise "state'' = ()" @@ (match get_state_for_tests state'' with () -> true) in

  (* add (tvc, SC_Constructor ctor_c) *)
  let ctor_c = make_c_constructor_simpl 3 None tvc C_unit [] in
  let state''' = add_constraint repr state'' (SC_Constructor ctor_c) in
  (* assert state''' = () because this index is not implemented yet, and its state is just a unit value *)
  let () = tst_assert ~raise "state''' = ()" @@ (match get_state_for_tests state''' with () -> true) in

  (* merging tvc to tva *)
  let merge_keys  : (type_variable, type_variable) merge_keys =
    let demoted_repr = tvc in
    let new_repr = tva in
    {
      map = (fun m -> UnionFind.ReprMap.alias ~demoted_repr ~new_repr m);
      set = (fun s -> UnionFind.ReprSet.alias ~demoted_repr ~new_repr s);
    }
  in
  let state'''' = merge_aliases merge_keys state''' in
  (* assert that c has been merged to a in state'''' *)
  (* state'''' = () because this index is not implemented yet, and its state is just a unit value *)
  let () = tst_assert ~raise "state'''' = ()" @@ (match get_state_for_tests state'''' with () -> true) in
  ()
  
