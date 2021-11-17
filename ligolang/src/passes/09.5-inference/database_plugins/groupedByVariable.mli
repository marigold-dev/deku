open Solver_types

module M : functor
  (Type_variable : sig type t end)
  (Type_variable_abstraction : TYPE_VARIABLE_ABSTRACTION(Type_variable).S) ->
sig
  open Type_variable_abstraction.Types
  include INDEXER_PLUGIN_TYPE(Type_variable)(Type_variable_abstraction).S
  val get_abs_by_lhs                   : 'type_variable -> 'type_variable t -> c_abs_simpl          MultiSet.t
  val get_constructors_by_lhs          : 'type_variable -> 'type_variable t -> c_constructor_simpl  MultiSet.t
  val get_rows_by_lhs                  : 'type_variable -> 'type_variable t -> c_row_simpl          MultiSet.t
  val get_polys_by_lhs                 : 'type_variable -> 'type_variable t -> c_poly_simpl         MultiSet.t
  val get_access_labels_by_result_type : 'type_variable -> 'type_variable t -> c_access_label_simpl MultiSet.t
  val get_access_labels_by_record_type : 'type_variable -> 'type_variable t -> c_access_label_simpl MultiSet.t

  type 'type_variable t_for_tests = {
    constructor : ('type_variable * c_constructor_simpl MultiSet.t) list ;
    poly        : ('type_variable * c_poly_simpl MultiSet.t) list ;
    row         : ('type_variable * c_row_simpl MultiSet.t) list ;
    access_label_by_result_type : ('type_variable * c_access_label_simpl MultiSet.t) list ;
    access_label_by_record_type : ('type_variable * c_access_label_simpl MultiSet.t) list ;
  }
  val bindings : 'type_variable t -> 'type_variable t_for_tests
end
