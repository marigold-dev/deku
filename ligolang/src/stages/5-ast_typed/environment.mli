open Types

type t = environment
type element = environment_element

val empty : t
val add_ez_binder : expression_variable -> type_expression -> t -> t
val add_ez_declaration : public:bool -> expression_variable -> expression -> known_attributes -> t -> t
val get_expr_environment : t -> expression_environment
val add_module : public:bool -> module_variable -> environment -> t -> t
val add_type : public:bool -> type_variable -> type_expression -> t -> t
val add_kind : type_variable -> unit -> t -> t
val add_type_var : type_variable -> unit -> t -> t
val of_list_type : (type_variable * type_expression) list -> t
val get_opt : ?other_module:bool -> expression_variable -> t -> element option
val get_type_opt : ?other_module:bool -> type_variable -> t -> type_expression option
val get_kind_opt : type_variable -> t -> unit option
val get_module_opt : ?other_module:bool -> module_variable -> t -> environment option
val get_constructor : label -> t -> (type_expression * type_expression) option
val get_constructor_parametric : label -> t -> (type_variable list * type_expression * type_expression) option
val get_record : row_element label_map -> t -> (type_variable option * rows) option
val get_sum : row_element label_map -> t -> rows option

module PP : sig
  open Format

  val list_sep_scope : (formatter -> 'a -> unit) -> formatter -> 'a list -> unit
  val environment : formatter -> environment -> unit

end
