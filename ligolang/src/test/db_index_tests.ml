open Test_helpers
open Db_index_assignment_tests
open Db_index_grouped_by_variable_tests
open Db_index_cycle_detection_topological_sort_tests
open Db_index_by_constraint_identifier_tests
open Db_index_typeclasses_constraining_tests

(* TODO: move this to another file *)
open Trace
open Ast_core.Types
open Solver_types
open Ast_core.Combinators
open Db_index_tests_common
module X (M : sig module Plugin_under_test : INDEXER_PLUGIN_TYPE(Solver_types.Type_variable)(Solver_types.Opaque_type_variable).S val same_state : raise:Main_errors.all raise -> type_variable Plugin_under_test.t -> type_variable Plugin_under_test.t -> unit end) = struct
  open Test_vars
  let invariant ~raise () =
    (* TODO: make this a functor taking the module and the same_state function *)
    let open M.Plugin_under_test in
    let repr : type_variable -> type_variable = fun tv -> tv in
    let merge_keys demoted_repr new_repr  : (type_variable, type_variable) merge_keys =
      {
        map = (fun m -> UnionFind.ReprMap.alias ~demoted_repr ~new_repr m);
        set = (fun s -> UnionFind.ReprSet.alias ~demoted_repr ~new_repr s);
      }
    in

    (* create empty state *)
    let istate = M.Plugin_under_test.create_state ~cmp:Ast_core.Compare.type_variable in
    let aux : _ t -> test_seq -> _ t = fun state seq ->
      match seq with
      | Add_cstr tv ->
        let tc = SC_Constructor (make_c_constructor_simpl 1 None tv C_unit []) in
        add_constraint repr state tc
      | Merge merge_keys -> merge_aliases merge_keys state
    in
    let state_a = List.fold_left ~f:aux ~init:istate
        [ Add_cstr tva ; Add_cstr tvb ; Add_cstr tvc ; Merge (merge_keys tva tvb) ; ]
    in
    let state_b = List.fold_left ~f:aux ~init:istate
        [ Add_cstr tva ; Add_cstr tvb ; Merge (merge_keys tva tvb) ; Add_cstr tvc ; ]
    in
    let () = M.same_state ~raise state_a state_b in
    ()
end

module Invariant_assignments                      = X(Assignments_tests)
module Invariant_grouped_by_variable              = X(Grouped_by_variable_tests)
module Invariant_cycle_detection_topological_sort = X(Cycle_detection_topological_sort_tests)
module Invariant_by_constraint_identifier         = X(By_constraint_identifier_tests)
module Invariant_typeclasses_constraining         = X(Typeclasses_constraining_tests)
(* End todo move *)

let main =
  test_suite "Indexers" @@
    [
      test "assignments" assignments ;
      test "grouped by variable" grouped_by_variable ;
      test "cycle detection topological sort" cycle_detection_topological_sort ;
      test "by constraint identifier" by_constraint_identifier ;
      test "typeclasses constraining" typeclasses_constraining ;
      test "invariant assignments" Invariant_assignments.invariant ;
      test "invariant grouped_by_variable" Invariant_grouped_by_variable.invariant ;
      test "invariant cycle detection topological sort" Invariant_cycle_detection_topological_sort.invariant ;
      test "invariant by constraint identifier" Invariant_by_constraint_identifier.invariant ;
      test "invariant typeclasses constraining" Invariant_typeclasses_constraining.invariant ;
    ]
