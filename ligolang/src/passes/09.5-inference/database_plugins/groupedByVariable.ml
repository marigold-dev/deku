(** Updates the dbs.grouped_by_variable field when new constraints are
   discovered.

    This field contains a map from type variables to lists of
   constraints that are related to that variable (in other words, the
   key appears in the equation).
 *)

open Solver_types

module M = functor
  (Type_variable : sig type t end)
  (Type_variable_abstraction : TYPE_VARIABLE_ABSTRACTION(Type_variable).S) ->
struct
  open Type_variable_abstraction.Types
  (* TODO: replace (did this to avoid merge conflict) *)
open Solver_types
open UnionFind
open Trace

(* the state is 3 maps from (unionfind) variables to constraints containing them *)

type 'typeVariable t = {
  abs         : ('typeVariable, c_abs_simpl         MultiSet.t) ReprMap.t ;
  constructor : ('typeVariable, c_constructor_simpl MultiSet.t) ReprMap.t ;
  poly        : ('typeVariable, c_poly_simpl        MultiSet.t) ReprMap.t ;
  row         : ('typeVariable, c_row_simpl         MultiSet.t) ReprMap.t ;
  access_label_by_result_type : ('typeVariable, c_access_label_simpl MultiSet.t) ReprMap.t ;
  access_label_by_record_type : ('typeVariable, c_access_label_simpl MultiSet.t) ReprMap.t ;
}
let pp type_variable ppf (state : _ t) =
  let open PP_helpers in
  Format.fprintf ppf "{ constructor = %a ; row = %a ; poly = %a }"
  (list_sep_d (pair type_variable (MultiSet.pp Type_variable_abstraction.PP.c_constructor_simpl_short))) (ReprMap.bindings state.constructor)
  (list_sep_d (pair type_variable (MultiSet.pp Type_variable_abstraction.PP.c_row_simpl_short))) (ReprMap.bindings state.row)
  (list_sep_d (pair type_variable (MultiSet.pp Type_variable_abstraction.PP.c_poly_simpl_short))) (ReprMap.bindings state.poly)


let create_state ~cmp =
  { abs         = ReprMap.create ~cmp ~merge:MultiSet.union ;
    constructor = ReprMap.create ~cmp ~merge:MultiSet.union ;
    poly        = ReprMap.create ~cmp ~merge:MultiSet.union ;
    row         = ReprMap.create ~cmp ~merge:MultiSet.union ;
    access_label_by_result_type = ReprMap.create ~cmp ~merge:MultiSet.union ;
    access_label_by_record_type = ReprMap.create ~cmp ~merge:MultiSet.union ;}

let update_add_to_constraint_set ~cmp c = function
    None -> MultiSet.add c (MultiSet.create ~cmp)
  | Some s -> MultiSet.add c s

let add_constraint ?debug repr (state : _ t) new_constraint =
  let _ = debug in
  match new_constraint with
    SC_Apply       _ -> state
  | SC_Abs         c -> { state with abs         = ReprMap.monotonic_update (repr c.tv) (update_add_to_constraint_set ~cmp:Type_variable_abstraction.Compare.c_abs_simpl         c) state.abs         }
  | SC_Constructor c -> { state with constructor = ReprMap.monotonic_update (repr c.tv) (update_add_to_constraint_set ~cmp:Type_variable_abstraction.Compare.c_constructor_simpl c) state.constructor }
  | SC_Row         c -> { state with row         = ReprMap.monotonic_update (repr c.tv) (update_add_to_constraint_set ~cmp:Type_variable_abstraction.Compare.c_row_simpl         c) state.row         }
  | SC_Poly        c -> { state with poly        = ReprMap.monotonic_update (repr c.tv) (update_add_to_constraint_set ~cmp:Type_variable_abstraction.Compare.c_poly_simpl        c) state.poly        }
  | SC_Access_label c ->
    { state with
      access_label_by_result_type = ReprMap.monotonic_update (repr c.tv) (update_add_to_constraint_set ~cmp:Type_variable_abstraction.Compare.c_access_label_simpl c) state.access_label_by_result_type ;
      access_label_by_record_type = ReprMap.monotonic_update (repr c.record_type) (update_add_to_constraint_set ~cmp:Type_variable_abstraction.Compare.c_access_label_simpl c) state.access_label_by_record_type
    }
  | SC_Typeclass   _ -> state (* Handled by a different indexer (typeclasses_constraining *)
  | SC_Alias       _ -> failwith "TODO: impossible: tc_alias handled in main solver loop"

(* Exception: Could not remove c because the database index entry is missing for the lhs of this constraint (e.g. a = b(c,d) : there is no entry for constraints which are constraining a) *)
exception CouldNotRemove of type_constraint_simpl

let update_remove_constraint_from_set tcs c = function
    None -> raise (CouldNotRemove tcs)
  | Some s -> MultiSet.remove c s


let remove_constraint ~(raise:Type_variable_abstraction.Errors.typer_error Trace.raise) _ repr (state : _ t) constraint_to_rm : _ =
  (* This update is "monotonic" as required by ReprMap + the solver.
     The `add` function of this indexer is only called once per
     constraint, and constraints are indexed by their lhs variable.
     Therefore, in any sequence of

     add c (* adds repr(v) → c in the multimap *)
     merge v other_v (* one or more merges + unrelated operations *)
     remove c (* removes new_repr(v) → c from the multimap *)

     the order of operations does not matter: the remove is always
     after the add, any merges before or after the add don't cause
     any problem. Indeed, the constraint appears only once in the
     multimap, and the removal will find it using the correct repr()
     at the time of removal. *)
  if Ast_core.Debug.debug_new_typer then Format.eprintf "in remove_constrant for groupedByVariable...%!";
  match
    (
      match constraint_to_rm with
        SC_Apply       _ -> state
      | SC_Abs         c -> { state with abs         = ReprMap.monotonic_update (repr c.tv) (update_remove_constraint_from_set constraint_to_rm c) state.abs         }
      | SC_Constructor c -> { state with constructor = ReprMap.monotonic_update (repr c.tv) (update_remove_constraint_from_set constraint_to_rm c) state.constructor }
      | SC_Row         c -> { state with row         = ReprMap.monotonic_update (repr c.tv) (update_remove_constraint_from_set constraint_to_rm c) state.row         }
      | SC_Poly        c -> { state with poly        = ReprMap.monotonic_update (repr c.tv) (update_remove_constraint_from_set constraint_to_rm c) state.poly        }
      | SC_Access_label c -> {
          state with
          access_label_by_result_type = ReprMap.monotonic_update (repr c.tv) (update_remove_constraint_from_set constraint_to_rm c) state.access_label_by_result_type ;
          access_label_by_record_type = ReprMap.monotonic_update (repr c.record_type) (update_remove_constraint_from_set constraint_to_rm c) state.access_label_by_record_type ;
        }
      | SC_Typeclass   _ -> state
      | SC_Alias       _ -> failwith "TODO: impossible: tc_alias handled in main solver loop and aliasing constraints cannot be removed"
    )
  with
  exception CouldNotRemove c -> raise.raise (`Typer_could_not_remove c)
  | result -> 
    if Ast_core.Debug.debug_new_typer then Format.eprintf "  ok\n%!";
    result

let merge_aliases =
  fun ?debug:_ updater { abs; constructor ; poly ; row ; access_label_by_result_type ; access_label_by_record_type } -> {
      abs                         = updater.map abs ;
      constructor = updater.map constructor ;
      poly       = updater.map poly ;
      row        = updater.map row ;
      access_label_by_result_type = updater.map access_label_by_result_type ;
      access_label_by_record_type = updater.map access_label_by_record_type ;
    }

let name = "grouped_by_variable"

let get_constructors_by_lhs : 'type_variable -> 'type_variable t -> c_constructor_simpl MultiSet.t =
  fun variable state ->
  match ReprMap.find_opt variable state.constructor with
    Some s -> s
  | None -> MultiSet.create ~cmp:Type_variable_abstraction.Compare.c_constructor_simpl

let get_rows_by_lhs : 'type_variable -> 'type_variable t -> c_row_simpl MultiSet.t =
  fun variable state ->
  match ReprMap.find_opt variable state.row with
    Some s -> s
  | None -> MultiSet.create ~cmp:Type_variable_abstraction.Compare.c_row_simpl

let get_polys_by_lhs : 'type_variable -> 'type_variable t -> c_poly_simpl MultiSet.t =
  fun variable state ->
  match ReprMap.find_opt variable state.poly with
    Some s -> s 
  | None -> MultiSet.create ~cmp:Type_variable_abstraction.Compare.c_poly_simpl

let get_access_labels_by_result_type : 'type_variable -> 'type_variable t -> c_access_label_simpl MultiSet.t =
  fun variable state ->
  match ReprMap.find_opt variable state.access_label_by_result_type with
    Some s -> s
  | None -> MultiSet.create ~cmp:Type_variable_abstraction.Compare.c_access_label_simpl

let get_access_labels_by_record_type : 'type_variable -> 'type_variable t -> c_access_label_simpl MultiSet.t =
  fun variable state ->
  match ReprMap.find_opt variable state.access_label_by_record_type with
    Some s -> s
  | None -> MultiSet.create ~cmp:Type_variable_abstraction.Compare.c_access_label_simpl

let get_abs_by_lhs : 'type_variable -> 'type_variable t -> c_abs_simpl MultiSet.t =
  fun variable state ->
  match ReprMap.find_opt variable state.abs with
    Some s -> s
  | None -> MultiSet.create ~cmp:Type_variable_abstraction.Compare.c_abs_simpl

type 'typeVariable t_for_tests = {
  constructor : ('typeVariable * c_constructor_simpl MultiSet.t) list ;
  poly        : ('typeVariable * c_poly_simpl MultiSet.t) list ;
  row         : ('typeVariable * c_row_simpl MultiSet.t) list ;
  access_label_by_result_type : ('typeVariable * c_access_label_simpl MultiSet.t) list ;
  access_label_by_record_type : ('typeVariable * c_access_label_simpl MultiSet.t) list ;
}

let bindings (state : _ t) : _ t_for_tests  = {
  constructor                 = ReprMap.bindings state.constructor                 ;
  row                         = ReprMap.bindings state.row                         ;
  poly                        = ReprMap.bindings state.poly                        ;
  access_label_by_result_type = ReprMap.bindings state.access_label_by_result_type ;
  access_label_by_record_type = ReprMap.bindings state.access_label_by_record_type ;
}
end
