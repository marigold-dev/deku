open Solver_types

(* data Indexers_plugins_fields (Ppt :: PerPluginType) = Indexers_plugins_fields {
     assignments       :: Ppt Assignments,
     groupedByVariable :: Ppt GroupedByVariable,
     …
   }
*)

module Assignments = Assignments.M(Solver_types.Type_variable)(Solver_types.Opaque_type_variable)
module GroupedByVariable = GroupedByVariable.M(Solver_types.Type_variable)(Solver_types.Opaque_type_variable)
module CycleDetectionTopologicalSort = CycleDetectionTopologicalSort.M(Solver_types.Type_variable)(Solver_types.Opaque_type_variable)
module ByConstraintIdentifier = ByConstraintIdentifier.M(Solver_types.Type_variable)(Solver_types.Opaque_type_variable)
module TypeclassesConstraining = TypeclassesConstraining.M(Solver_types.Type_variable)(Solver_types.Opaque_type_variable)
module Typeclasses_using_as_unbound_var = Typeclasses_using_as_unbound_var.M(Solver_types.Type_variable)(Solver_types.Opaque_type_variable)
module Typeclasses_using_as_function_on_root = Typeclasses_using_as_function_on_root.M(Solver_types.Type_variable)(Solver_types.Opaque_type_variable)

(* TODO: this is probably hidden by its signature somewhere? *)
module Indexers_plugins_fields = functor (Ppt : PerPluginType) -> struct
  type flds = <
    assignments                      : Ppt(Assignments).t ;
    grouped_by_variable              : Ppt(GroupedByVariable).t ;
    cycle_detection_topological_sort : Ppt(CycleDetectionTopologicalSort).t ;
    by_constraint_identifier         : Ppt(ByConstraintIdentifier).t ;
    typeclasses_constraining         : Ppt(TypeclassesConstraining).t ;
    typeclasses_using_as_unbound_var : Ppt(Typeclasses_using_as_unbound_var).t ;
    typeclasses_using_as_function_on_root : Ppt(Typeclasses_using_as_function_on_root).t ;
  >

  module Assignments = Assignments
  let assignments flds = (flds :> <assignments:_>)
end

(* TODO: try removing this _ workaround *)
module Indexers_plugins_fields_ = Indexers_plugins_fields

(* mapPlugins :: (F : MappedFunction) → (Indexers_plugins_fields F.MakeIn) → (Indexers_plugins_fields F.MakeOut) *)
module Map_indexer_plugins = functor (F : Mapped_function) -> struct
  let f :
    raise:Typer_common.Errors.typer_error Trace.raise ->
    F.extra_args ->
    Indexers_plugins_fields(F.MakeInType).flds ->
    Indexers_plugins_fields(F.MakeOutType).flds
    = fun ~raise extra_args fieldsIn ->
      let assignments                           = (let module F = F.F(Assignments)                           in F.f ~raise "assign" extra_args fieldsIn#assignments)                      in
      let grouped_by_variable                   = (let module F = F.F(GroupedByVariable)                     in F.f ~raise "g by v" extra_args fieldsIn#grouped_by_variable)              in
      let cycle_detection_topological_sort      = (let module F = F.F(CycleDetectionTopologicalSort)         in F.f ~raise "c topo" extra_args fieldsIn#cycle_detection_topological_sort) in
      let by_constraint_identifier              = (let module F = F.F(ByConstraintIdentifier)                in F.f ~raise "by  id" extra_args fieldsIn#by_constraint_identifier)         in
      let typeclasses_constraining              = (let module F = F.F(TypeclassesConstraining)               in F.f ~raise "tc con" extra_args fieldsIn#typeclasses_constraining)         in
      let typeclasses_using_as_unbound_var      = (let module F = F.F(Typeclasses_using_as_unbound_var)      in F.f ~raise "tc unb" extra_args fieldsIn#typeclasses_using_as_unbound_var) in
      let typeclasses_using_as_function_on_root = (let module F = F.F(Typeclasses_using_as_function_on_root) in F.f ~raise "tc unb" extra_args fieldsIn#typeclasses_using_as_function_on_root) in
      (object
        method assignments                      = assignments
        method grouped_by_variable              = grouped_by_variable
        method cycle_detection_topological_sort = cycle_detection_topological_sort
        method by_constraint_identifier         = by_constraint_identifier
        method typeclasses_constraining         = typeclasses_constraining
        method typeclasses_using_as_unbound_var = typeclasses_using_as_unbound_var
        method typeclasses_using_as_function_on_root = typeclasses_using_as_function_on_root
      end)
end

(* A value containing an empty (dummy) unit associated each plugin
   name This allows us to use `map' to discard this `unit' and
   e.g. initialize each plugin. *)
type indexers_plugins_fields_unit = Indexers_plugins_fields(PerPluginUnit).flds
let indexers_plugins_fields_unit : indexers_plugins_fields_unit = object
  method assignments                      = () ;
  method grouped_by_variable              = () ;
  method cycle_detection_topological_sort = () ;
  method by_constraint_identifier         = () ;
  method typeclasses_constraining         = () ;
  method typeclasses_using_as_unbound_var = () ;
  method typeclasses_using_as_function_on_root = () ;
end

module All_plugins = All_plugins
