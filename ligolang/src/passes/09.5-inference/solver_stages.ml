module Core = Typesystem.Core
module Map = RedBlackTrees.PolyMap
module Set = RedBlackTrees.PolySet
module UF = UnionFind.Poly2
open Solver_types

module M(Plugins : Plugins)(Solver : sig
    module Indexers_plugins_states : module type of Plugins.Indexers.Indexers_plugins_fields(PerPluginState)
    type indexers_plugins_states = Plugins.Indexers.Indexers_plugins_fields(PerPluginState).flds
    type nonrec typer_state = indexers_plugins_states Solver_types.typer_state
    (*type indexers_plugins_fields_unit = Plugins.Indexers.Indexers_plugins_fields(PerPluginUnit).flds*)
    module Worklist_and_pending : module type of Worklist_and_pending.M(struct type nonrec indexers_plugins_states = indexers_plugins_states end)
  end) = struct

  open Solver
  open Worklist_and_pending

  let mk_repr state x = UnionFind.Poly2.repr x state.aliases

  let filter_already_added ~raise:_ ((state : typer_state), type_constraint) =
    if PolySet.mem type_constraint state.added_constraints
    then (state, Worklist.empty)
    else ({ state with added_constraints = PolySet.add type_constraint state.added_constraints},
          { Worklist.empty with pending_filtered_not_already_added_constraints = Pending.singleton type_constraint })

  let simplify_constraint ~raise:_ (state, new_constraint) =
    let constraint_simplified = Simplifier.type_constraint_simpl new_constraint in
    (* Format.eprintf "Simplify constraint : %a\n, new_constraint : %a\n%!"
      Ast_typed.PP.type_constraint_short new_constraint
      (PP_helpers.list_sep_d Ast_typed.PP.type_constraint_simpl_short) constraint_simplified; *)
    (state, { Worklist.empty with pending_type_constraint_simpl = Pending.of_list constraint_simplified })

  let split_aliases ~raise:_ (state, new_constraint) =
    match new_constraint with
      Ast_core.Types.SC_Alias c_alias ->
      (state, { Worklist.empty with pending_c_alias = Pending.singleton c_alias })
    | non_alias ->
      (state, { Worklist.empty with pending_non_alias = Pending.singleton non_alias })
  
end
