open Solver_types

module M = functor
  (Type_variable : sig type t end)
  (Type_variable_abstraction : TYPE_VARIABLE_ABSTRACTION(Type_variable).S) ->
struct
  open Type_variable_abstraction
  open Type_variable_abstraction.Types
  open UnionFind

  type 'typeVariable t = ('typeVariable, c_typeclass_simpl MultiSet.t) ReprMap.t
  type ('type_variable, 'a) state = < typeclasses_using_as_unbound_var : 'type_variable t ; .. > as 'a

  let create_state ~cmp =
    let merge = MultiSet.union in
    ReprMap.create ~cmp ~merge

  let repr_map_add_to_set ~cmp repr_tv x state =
    let add_to_set = function
        Some set -> MultiSet.add x set
      | None -> MultiSet.add x (MultiSet.create ~cmp) in
    ReprMap.monotonic_update repr_tv add_to_set state

  let p_variable_cells c = List.filter_map
      ~f:(function { Location.wrap_content = P_variable v } -> Some v | _ -> None)
      (List.concat c.tc)

  let register_typeclasses_using_as_unbound_var : _ -> c_typeclass_simpl -> _ t -> _ t = fun repr c state ->
    List.fold_left
      ~f:(fun state tv -> repr_map_add_to_set ~cmp:Compare.c_typeclass_simpl (repr tv) c state)
      ~init:state
      (p_variable_cells c)

  let add_constraint ?debug repr state new_constraint =
    let _ = debug in
    match new_constraint with
    | SC_Typeclass c -> register_typeclasses_using_as_unbound_var repr c state
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
          (p_variable_cells constraint_to_remove) in
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

  let name = "typeclasses_using_as_unbound_var"

  let get_state_for_tests state = state

  module type STATE = sig val typeclasses_using_as_unbound_var : Type_variable.t t end
  let get tv (module State : STATE) =
    Option.value ~default:(MultiSet.create ~cmp:Type_variable_abstraction.Compare.c_typeclass_simpl)
    @@ ReprMap.find_opt tv State.typeclasses_using_as_unbound_var

  let get_list tv state =
    MultiSet.elements @@ get tv state
end
