open Test_helpers
module Core = Typesystem.Core
open Ast_core.Types
(* open Solver_types *)
(* open Typer_common.Errors *)
open Trace
open Heuristic_common

open Inference.Heuristic_specialize1

let rv : type_value -> row_value = fun v -> { associated_value = v; michelson_annotation = None; decl_pos = 0}
let rv' : type_variable -> row_variable = fun v -> { associated_variable = v; michelson_annotation = None; decl_pos = 0}

let c1 = constraint_ 1L n == forall v [] @=> var v @-> p_row C_record { x = rv int; y = rv (var v) } @-> map(var v,int)
let c2 = constraint_ 2L n' == ctor C_arrow[p; q]
let c3 = constraint_ 3L o == ctor C_string[]
let c4 = constraint_ 4L p == row C_record { x = rv' m ; y = rv' m }
let c5 = constraint_ 5L q == ctor C_map[o; r]
           
let c6 = constraint_ 6L n == ctor C_arrow[o; n']

let check_specialize1_result fresh (actual : update list) expected =
  match actual with
    [{add_constraints=[added_constraint]; remove_constraints = []; proof_trace = _}] ->
    let _ = fresh, expected, added_constraint in
    (failwith "TODO: compare added_constraint and expected, and check that all instances of fresh in expected are matched with the same variable in added_constraint")
  | _ -> (failwith "test failed for specialize1 expected …expected but got …actual")

module Map = Database_plugins.GroupedByVariable
let selector_test ~raise : _ -> _ -> unit -> unit =
  fun selector comparator () ->
  (*create a state :)
    state = [
      (constraint 1L n = poly ∀ v, v -> record { x = int ; y = v } -> map(v,int) )
      (constraint 2L n' =     p -> q)
      (constraint 3L o = string)
      (constraint 4L p = record { x = r ; y = o })
      (constraint 5L q = map(o,r)
    ] *)
  let state = [ !. c1; !. c2; !. c3; !. c4; !. c5 ] in
  (* pass a new constraint to the selector:
      (constraint 6L n = o -> n')
  *)
  let repr = fun x -> x in
  let result = selector repr (!. c6) (grouped_by_variable state) in
  (* check that the selector returns a list containing the pair of constraints (1L, 6L) *)
  let comparator = List.compare comparator in
  tst_assert ~raise "expected the selector to return a list containg the pair of constraints (1L,6L) or (6L,1L)"
    (comparator result [{ a_k_var = !.. c6; poly = !.. c1 }] = 1)

let propagator_test ~raise : _ -> unit -> unit =
  fun propagator () ->
  (* call the propagator with the pair of constraints
      (constraint 1L m = poly ∀ v, v -> record { x = int ; y = v } -> map(v,int))
      (constraint 6L n = o -> n')
   *)
  let result = trace ~raise Main_errors.inference_tracer @@
    propagator { a_k_var = !.. c6; poly = !.. c1 } @@ fun x -> x in
  (* check that the propagator returns exactly this constraint
     (left/right in the equality is not important, variable "fresh" is not important):
     n = fresh -> record { x = int ; y = fresh } -> map(fresh,int) *)
  let fresh = Var.fresh ~name:"fresh" () in
  check_specialize1_result fresh (result : update list) (n === var fresh @-> p_row C_record { x = rv int ; y = rv (var fresh) } @-> map(var fresh, int))

let selector_test2 ~raise:_ : _ -> _ -> unit -> unit =
  fun _selector _ () ->
  (*create a state :)
    state = [
      (constraint 1L a = int)
      (constraint 2L b = record { x = a ; y = a })
      (constraint 3L c = map(a, b))
      (constraint 4L d = poly … whatever)
    ] *)
  (* pass a new constraint to the selector:
      (constraint 5L c = map(f, b)) (*  THIS was changed to f,b instead of f,g in the other test *)
  *)
  (* check that the selector returns a list containing the pair of constraints (3L, 5L) or (5L, 3L) *)
  ()

let propagator_test2 ~raise:_ : _ -> unit -> unit =
  fun _propagator () ->
  (* call the propagator with the pair of constraints
      (constraint 3L c = map(a, b))
      (constraint 5L c = map(f, b))
   *)
  (* check that the propagator returns these constraints
     (order is not important, order left/right in the equality is not
     important):
     a = f
     b = b (optional, if it is not returned it is not an error, just look which version (with/without) allows this test to pass) *)
  ()

let main =
  let open Inference.Heuristic_specialize1 in
  test_suite "Typer : specialize1 heuristic" @@
    [
      (* test "selector" (selector_test selector comparator) ;
      test "propagator" (propagator_test propagator) ; *)
      test "selector2" (selector_test2 selector comparator) ;
      test "propagator2" (propagator_test2 propagator) ;
    ]
