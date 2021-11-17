open Test_helpers
module Core = Typesystem.Core
open Ast_core.Types
open Solver_types
open Typer_common.Errors
open Trace
open Heuristic_common

open Inference.Heuristic_break_ctor

let c1 = constraint_ 1L m == ctor C_int[]
let c2 = constraint_ 2L n == row C_record { x = {associated_variable=m;michelson_annotation=None;decl_pos=0} ; y = {associated_variable=m;michelson_annotation=None;decl_pos=1} }
let c3 = constraint_ 3L o == ctor C_map[m; n]
let c4 = constraint_ 4L p == forall x [] @=> var x @-> int @-> map(var x,int)
let c5 = constraint_ 5L o == ctor C_map[y; z]
let c6 = constraint_ 6L o == ctor C_map[y; n]

module Map = Database_plugins.GroupedByVariable
let selector_test ~raise : _ -> _ -> unit -> unit =
  fun selector comparator () ->
  (* create a state:
      state = [
        (constraint 1L m = int)
        (constraint 2L n = record { x = m ; y = m })
        (constraint 3L o = map(m, n))
        (constraint 4L p = poly ∀ x, x -> int -> map(x,int))
      ] *)
  let state = [ !. c1; !. c2; !. c3; !. c4; ] in

  let repr = fun x -> x in
  (* pass a new constraint to the selector:
      (constraint 5L o = map(y, z)) *)
  let result = selector repr (!. c5) (grouped_by_variable state) in
  (* Check against expected *)
  let comparator = List.compare comparator in
  tst_assert ~raise "expected the selector to return a list containg the pair of constraints (3L,5L) or (5L,3L)"
    (comparator result [{ a_k_var = `Constructor !.. c3; a_k'_var' = `Constructor !.. c5 }] = 1 ||
     comparator result [{ a_k_var = `Constructor !.. c5; a_k'_var' = `Constructor !.. c3 }] = 1)
let propagator_test ~raise : (selector_output, typer_error) propagator -> unit -> unit =
  fun propagator () ->
  (* call the propagator with the pair of constraints
      (constraint 3L c = map(m, n))
      (constraint 5L c = map(y, z)) *)
  let repr = fun x -> x in
  let result = trace ~raise Main_errors.inference_tracer @@
    propagator { a_k_var = `Constructor !.. c3; a_k'_var' = `Constructor !.. c5 } repr in
  (* check that the propagator returns these constraints
     (order is not important, order left/right in the equality is not
     important):
     m = y
     n = z *)
  check_list_of_equalities ~raise (result : update list) [(m,y); (n,z)]

let selector_test2 ~raise : _ -> _ -> unit -> unit =
  fun selector comparator () ->
  (* create a state *)
  let state = [ !. c1; !. c2; !. c3; !. c4; ] in
  let repr = fun x -> x in
  (* pass a new constraint to the selector:
     THIS was changed to y,n instead of y,z in the other test
     (constraint 6L o = map(y, n)) *)
  let result = selector repr (!. c6) (grouped_by_variable state) in
  (* Check against expected *)
  let comparator = List.compare comparator in
  tst_assert ~raise "expected the selector to return a list containg the pair of constraints (3L,6L) or (6L,3L)"
    (comparator result [{ a_k_var = `Constructor !.. c3; a_k'_var' = `Constructor !.. c6 }] = 1 ||
     comparator result [{ a_k_var = `Constructor !.. c6; a_k'_var' = `Constructor !.. c3 }] = 1)

let propagator_test2 ~raise : _ -> unit -> unit =
  fun propagator () ->
  let repr = fun x -> x in
  (* call the propagator with the pair of constraints
      (constraint 3L c = map(m, n))
      (constraint 6L c = map(y, n)) THIS was changed to y,n instead of y,z in the other test *)
  let result = trace ~raise Main_errors.inference_tracer @@
    fun ~raise -> propagator ~raise { a_k_var = `Constructor !.. c3; a_k'_var' = `Constructor !.. c5 } repr in
  (* check that the propagator returns these constraints
     (order is not important, order left/right in the equality is not
     important):
     m = y
     n = n (optional, if it is not returned it is not an error, just look which version (with/without) allows this test to pass) *)
  check_list_of_equalities ~raise (result : update list) [(m,y);(n,z)]

let main =
  let open Inference.Heuristic_break_ctor in
  test_suite "Typer : ctor break heuristic" @@
    [
      test "selector" (selector_test selector comparator) ;
      test "propagator" (propagator_test propagator) ;
      test "selector2" (selector_test2 selector comparator) ;
      test "propagator2" (propagator_test2 propagator) ;
    ]
