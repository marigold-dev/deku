open Solver_types

module M : functor
  (Type_variable : sig type t end)
  (Type_variable_abstraction : TYPE_VARIABLE_ABSTRACTION(Type_variable).S) ->
sig
  open Type_variable_abstraction.Types
  include INDEXER_PLUGIN_TYPE(Type_variable)(Type_variable_abstraction).S
  val find_opt : constraint_identifier -> 'type_variable t -> c_typeclass_simpl option
  val get_state_for_tests : _ t -> (constraint_identifier, c_typeclass_simpl) PolyMap.t
end
