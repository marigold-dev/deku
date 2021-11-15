open Solver_types

module M = functor
  (Type_variable : sig type t end)
  (Type_variable_abstraction : TYPE_VARIABLE_ABSTRACTION(Type_variable).S) ->
struct
  open Type_variable_abstraction.Types
open UnionFind

type 'typeVariable t = ('typeVariable, c_typeclass_simpl MultiSet.t) ReprMap.t
type ('type_variable, 'a) state = < typeclasses_constraining : 'type_variable t ; .. > as 'a

let create_state ~cmp =
  let merge = MultiSet.union in
  ReprMap.create ~cmp ~merge

let register_typeclasses_constraining : _ -> c_typeclass_simpl -> _ t -> _ t =
  fun repr c state ->
  let aux' = function
      Some set -> MultiSet.add c set
    | None -> MultiSet.add c (MultiSet.create ~cmp:Type_variable_abstraction.Compare.c_typeclass_simpl) in
  let aux state tv =
    ReprMap.monotonic_update (repr tv) aux' state in
  List.fold_left
    ~f:aux
    ~init:state
    (List.rev c.args)

let add_constraint ?debug repr state new_constraint =
  let _ = debug in
  match new_constraint with
  | SC_Typeclass c -> register_typeclasses_constraining repr c state
  | _ -> state

let remove_constraint ~raise:_ printer repr state constraint_to_remove =
  if Ast_core.Debug.debug_new_typer then Format.eprintf "remove_constraint for typeclassesConstraining.... \n%!";
    match constraint_to_remove with
  | Type_variable_abstraction.Types.SC_Typeclass constraint_to_remove ->
    let aux' = function
        Some set -> MultiSet.remove constraint_to_remove set
      | None -> 
        if Ast_core.Debug.debug_new_typer then Format.eprintf "ERROR: No set linked to tv"; (* TODO: should probably fail at this point. *)
        MultiSet.create ~cmp:Type_variable_abstraction.Compare.c_typeclass_simpl in
    let aux typeclasses_constrained_by tv =
      if Ast_core.Debug.debug_new_typer then Format.eprintf "In aux with tv : %a and repr tv : %a\n%!" Type_variable_abstraction.PP.type_variable tv printer @@ repr tv;
      ReprMap.monotonic_update (repr tv) aux' typeclasses_constrained_by in
    let state =
      List.fold_left
        ~f:aux
        ~init:state
        (List.rev constraint_to_remove.args) in
        if Ast_core.Debug.debug_new_typer then Format.eprintf "  ok\n%!";
    state
  | _ -> 
    if Ast_core.Debug.debug_new_typer then Format.eprintf "  ok\n%!";
    state

let merge_aliases : 'old 'new_ . ?debug:(Format.formatter -> 'new_ t -> unit) -> ('old, 'new_) merge_keys -> 'old t -> 'new_ t =
  fun ?debug:_ merge_keys state -> 
    let state = merge_keys.map state in
    state

let pp type_variable ppf state =
  let open PP_helpers in
  list_sep_d
    (pair
       type_variable
       (fun ppf set -> list_sep_d Type_variable_abstraction.PP.c_typeclass_simpl_short ppf (MultiSet.elements set)))
    ppf
    (ReprMap.bindings state)

let name = "typeclasses_constraining"

let get_state_for_tests state = state

module type STATE = sig val typeclasses_constraining : Type_variable.t t end

let get tv (module State : STATE) =
  Option.value ~default:(MultiSet.create ~cmp:Type_variable_abstraction.Compare.c_typeclass_simpl)
  @@ ReprMap.find_opt tv State.typeclasses_constraining

let get_list tv state =
  MultiSet.elements @@ get tv state
end
