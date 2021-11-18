module Core = Typesystem.Core
module Map = RedBlackTrees.PolyMap
module Set = RedBlackTrees.PolySet
module UF = UnionFind.Poly2
open Ast_core.Types
open Solver_types
open UnionFind

(* Function which merges all aliases withing a single plugin's state *)
module MergeAliases = struct
  type extra_args = type_variable UnionFind.Poly2.changed_reprs
  module MakeInType = PerPluginState
  module MakeOutType = PerPluginState
  module Monad = NoMonad
  module F(Indexer_plugin : INDEXER_PLUGIN_TYPE(Solver_types.Type_variable)(Solver_types.Opaque_type_variable).S) = struct
    let f ~raise:_ _ UnionFind.Poly2.{ demoted_repr ; new_repr } state =
      let merge_keys = {
        map = (fun m -> ReprMap.alias ~debug:(fun ppf (a,_) -> Ast_typed.PP.type_variable ppf a) ~demoted_repr ~new_repr m);
        set = (fun s -> ReprSet.alias ~demoted_repr ~new_repr s);
        (* var = (fun a -> if Var.compare a demoted_repr = 0 then new_repr else a) *)
      }
      in Indexer_plugin.merge_aliases ~debug:(Indexer_plugin.pp Ast_typed.PP.type_variable) merge_keys state
  end
end
(* check module matches signature without hiding its contents *)
let _ = (module MergeAliases : Mapped_function)

(* Function which adds a constraint to single indexer plugin's state *)
module AddConstraint = struct
  type extra_args = (type_variable -> type_variable) * type_constraint_simpl
  module MakeInType = PerPluginState
  module MakeOutType = PerPluginState
  module Monad = NoMonad
  module F(Indexer_plugin : INDEXER_PLUGIN_TYPE(Solver_types.Type_variable)(Solver_types.Opaque_type_variable).S) = struct
    let f ~raise:_ _ (repr, constraint_) state =
      (* Format.eprintf "In AddConstraint for %s and constraint %a\n%!" name PP.type_constraint_ constraint_; *)
      Indexer_plugin.add_constraint repr state constraint_
  end
end
(* check module matches signature without hiding its contents *)
let _ = (module AddConstraint : Mapped_function)

(* Function which merges all aliases withing a single plugin's state *)
module RemoveConstraint = struct
  (* TODO: check this API to see if we're giving too much flexibility to the plugin *)
  type extra_args = ((type_variable -> type_variable) * type_constraint_simpl)
  module MakeInType = PerPluginState
  module MakeOutType = PerPluginState
  module F(Indexer_plugin : INDEXER_PLUGIN_TYPE(Solver_types.Type_variable)(Solver_types.Opaque_type_variable).S) = struct
    let f ~raise _ (repr, to_remove) state =
      (* Format.eprintf "In Remove Constraint in %s for %a\n%!" name PP.type_constraint_ to_remove; *)
      Indexer_plugin.remove_constraint ~raise Ast_typed.PP.type_variable repr state to_remove
  end
end
(* check module matches signature without hiding its contents *)
let _ = (module RemoveConstraint : Mapped_function)

(* Function which creates a plugin's initial state *)
module CreateState = struct
  type extra_args = unit
  module MakeInType = PerPluginUnit
  module MakeOutType = PerPluginState
  module Monad = NoMonad
  module F(Indexer_plugin : INDEXER_PLUGIN_TYPE(Solver_types.Type_variable)(Solver_types.Opaque_type_variable).S) = struct
    let f ~raise:_ _ () (() as _state) = Indexer_plugin.create_state ~cmp:Ast_typed.Compare.type_variable
  end
end
(* check module matches signature without hiding its contents *)
let _ = (module CreateState : Mapped_function)

(* Function which calls the plugin's pretty-printer and appends it to an accumulator string *)
module PPPlugin = struct
  type extra_args = Format.formatter
  module MakeInType = PerPluginState
  module MakeOutType = PerPluginUnit
  module Monad = NoMonad
  module F(Indexer_plugin : INDEXER_PLUGIN_TYPE(Solver_types.Type_variable)(Solver_types.Opaque_type_variable).S) = struct
    let f ~raise:_ _ ppf state =
      Format.fprintf ppf "%s =@ @[<hv 2> %a @] ;@ " Indexer_plugin.name (Indexer_plugin.pp Var.pp) state
  end
end
(* check module matches signature without hiding its contents *)
let _ = (module PPPlugin : Mapped_function)

let init_propagator_heuristic (Heuristic_plugin plugin) =
  Heuristic_state { plugin; already_selected = Set.create ~cmp:plugin.comparator }
