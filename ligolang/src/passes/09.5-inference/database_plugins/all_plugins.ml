(* OCaml/dune hide the contents of a folder unless they are
   re-exported… this is just to be able to access the modules from
   outside. This has nothing to do with the plugin architecture. *)
open Solver_types
module M = functor
  (Type_variable : sig type t end)
  (Type_variable_abstraction : TYPE_VARIABLE_ABSTRACTION(Type_variable).S) ->
struct
  module Assignments                           = Assignments.M(Type_variable)(Type_variable_abstraction)
  module Grouped_by_variable                   = GroupedByVariable.M(Type_variable)(Type_variable_abstraction)
  module Cycle_detection_topological_Sort      = CycleDetectionTopologicalSort.M(Type_variable)(Type_variable_abstraction)
  module By_constraint_identifier              = ByConstraintIdentifier.M(Type_variable)(Type_variable_abstraction)
  module Typeclasses_constraining              = TypeclassesConstraining.M(Type_variable)(Type_variable_abstraction)
  module Typeclasses_using_as_unbound_var      = Typeclasses_using_as_unbound_var.M(Type_variable)(Type_variable_abstraction)
  module Typeclasses_using_as_function_on_root = Typeclasses_using_as_function_on_root.M(Type_variable)(Type_variable_abstraction)
end

include M(Solver_types.Type_variable)(Solver_types.Opaque_type_variable)
