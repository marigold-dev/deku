open Ast_core.Types
open Solver_types

module Assignments : module type of Assignments.M(Solver_types.Type_variable)(Solver_types.Opaque_type_variable)
module GroupedByVariable : module type of GroupedByVariable.M(Solver_types.Type_variable)(Solver_types.Opaque_type_variable)
module CycleDetectionTopologicalSort : module type of CycleDetectionTopologicalSort.M(Solver_types.Type_variable)(Solver_types.Opaque_type_variable)
module ByConstraintIdentifier : module type of ByConstraintIdentifier.M(Solver_types.Type_variable)(Solver_types.Opaque_type_variable)
module TypeclassesConstraining : module type of TypeclassesConstraining.M(Solver_types.Type_variable)(Solver_types.Opaque_type_variable)
module Typeclasses_using_as_unbound_var : module type of Typeclasses_using_as_unbound_var.M(Solver_types.Type_variable)(Solver_types.Opaque_type_variable)
module Typeclasses_using_as_function_on_root : module type of Typeclasses_using_as_function_on_root.M(Solver_types.Type_variable)(Solver_types.Opaque_type_variable)

module Indexers_plugins_fields_ (Ppt : PerPluginType) : sig
  type flds = <
    assignments                      : Ppt(Assignments).t ;
    grouped_by_variable              : Ppt(GroupedByVariable).t ;
    cycle_detection_topological_sort : Ppt(CycleDetectionTopologicalSort).t ;
    by_constraint_identifier         : Ppt(ByConstraintIdentifier).t ;
    typeclasses_constraining         : Ppt(TypeclassesConstraining).t ;
    typeclasses_using_as_unbound_var : Ppt(Typeclasses_using_as_unbound_var).t ;
    typeclasses_using_as_function_on_root : Ppt(Typeclasses_using_as_function_on_root).t ;
  >

  module Assignments : sig
    type 'typeVariable t
    val find_opt : 'type_variable -> 'type_variable t -> constructor_or_row option
    val bindings : 'type_variable t -> ('type_variable * constructor_or_row) list
    val pp : (Format.formatter -> 'typeVariable -> unit) -> Format.formatter -> 'typeVariable t -> unit
  end
  val assignments : flds -> < assignments : Ppt(Assignments).t >
end

include Solver_types.IndexerPlugins
  (* TODO: do we need this & the definition above? *)
  with module Indexers_plugins_fields = Indexers_plugins_fields_


(* OCaml/dune hide the contents of a folder unless they are
   re-exported… this is just to be able to access the modules from
   outside. This has nothing to do with the plugin architecture. *)
module All_plugins = All_plugins
