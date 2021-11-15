open Solver_types

module M : functor
  (Type_variable : sig type t end)
  (Type_variable_abstraction : TYPE_VARIABLE_ABSTRACTION(Type_variable).S) ->
sig
  open Type_variable_abstraction.Types
  include INDEXER_PLUGIN_TYPE(Type_variable)(Type_variable_abstraction).S
  type ('type_variable, 'a) state = < typeclasses_using_as_function_on_root : 'type_variable t ; .. > as 'a

  module type STATE = sig val typeclasses_using_as_function_on_root : Type_variable.t t end
  val get : Type_variable.t -> (module STATE) -> c_typeclass_simpl MultiSet.t
  val get_list : Type_variable.t -> (module STATE) -> c_typeclass_simpl list
  val functions_on_roots : (Type_variable.t -> 'type_variable) -> c_typeclass_simpl -> 'type_variable list

  val get_state_for_tests : 'type_variable t -> ('type_variable, c_typeclass_simpl MultiSet.t) UnionFind.ReprMap.t
end
