open Solver_types

module M : functor
  (Type_variable : sig type t end)
  (Type_variable_abstraction : TYPE_VARIABLE_ABSTRACTION(Type_variable).S) ->
sig
  include INDEXER_PLUGIN_TYPE(Type_variable)(Type_variable_abstraction).S
  val get_state_for_tests : 'type_variable t -> unit
end
