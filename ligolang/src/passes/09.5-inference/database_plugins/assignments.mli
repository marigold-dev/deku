open Solver_types

module M : functor
  (Type_variable : sig type t end)
  (Type_variable_abstraction : TYPE_VARIABLE_ABSTRACTION(Type_variable).S) ->
sig
  open Type_variable_abstraction.Types
  include INDEXER_PLUGIN_TYPE(Type_variable)(Type_variable_abstraction).S
  val find_opt : 'type_variable -> 'type_variable t -> constructor_or_row option
  val bindings : 'type_variable t -> ('type_variable * constructor_or_row) list
end

