open Types

module Expression : sig
  type t' = expression_content
  type t = expression

  val get_content : t -> t' 
  val get_type : t -> type_expression
  (*
  val is_toplevel : t -> bool 
*)
  val make_t   : ?loc:Location.t -> type_content -> type_expression
  val make     : ?loc:Location.t -> t' -> type_expression -> t
  val make_tpl : ?loc:Location.t -> t' * type_expression -> t

  val pair : t -> t -> t'
end

val get_bool : value -> bool option
val get_int : value -> Z.t option
val get_nat : value -> Z.t option
val get_mutez : value -> Z.t option
val get_timestamp : value -> Z.t option
val get_string : value -> string option
val get_bytes : value -> bytes option
val get_unit : value -> unit option
val get_option : value -> value option option
val get_map : value -> (value * value) list option
val get_big_map : value -> ( value * value ) list option
val get_list : value -> value list option
val get_set : value -> value list option
val get_function_with_ty : expression -> ( anon_function * ( type_expression * type_expression) ) option
val get_function : expression -> anon_function option
val get_t_function : type_expression -> ( type_expression * type_expression ) option
val get_t_option : type_expression -> type_expression option
val get_pair : value -> ( value * value ) option
val get_t_pair : type_expression -> ( type_expression * type_expression ) option
val get_t_or : type_expression -> ( type_expression * type_expression ) option
val get_t_map : type_expression -> ( type_expression * type_expression ) option
val get_t_big_map : type_expression -> ( type_expression * type_expression ) option
val get_t_list : type_expression -> type_expression option
val get_t_set : type_expression -> type_expression option
val get_t_collection : type_expression -> type_expression option
val get_left : value -> value option
val get_right : value -> value option
val get_ticket : value -> (value*value) option
val get_or : value -> ( bool * value ) option
(*
val wrong_type : string -> type_expression -> unit -> error
*)
val get_t_left : type_expression -> type_expression option
val get_t_right : type_expression -> type_expression option
val get_t_contract : type_expression -> type_expression option
val get_t_operation : type_expression -> type_expression option
val get_t_sapling_state : type_expression -> Z.t option
val get_operation : value -> bytes option

val t_int      : ?loc:Location.t -> unit -> type_expression 
val t_unit     : ?loc:Location.t -> unit -> type_expression 
val t_nat      : ?loc:Location.t -> unit -> type_expression 
val t_function : ?loc:Location.t -> type_expression -> type_expression -> type_expression
val t_pair     : ?loc:Location.t -> type_expression annotated -> type_expression annotated -> type_expression
val t_union    : ?loc:Location.t -> type_expression annotated -> type_expression annotated -> type_expression
val t_tuple : ?loc:Location.t -> type_expression annotated list -> type_expression
(*
val quote : string -> type_expression -> type_expression -> Expression.t -> anon_function


val e_int : Expression.t' -> Expression.t
*)
val e_unit    : ?loc:Location.t -> unit -> Expression.t
val e_var_int : ?loc:Location.t -> expression_variable -> Expression.t
val e_let_in  : ?loc:Location.t -> expression_variable -> type_expression -> inline -> Expression.t -> Expression.t -> Expression.t

(*
val ez_e_return : Expression.t -> Expression.t
*)
val d_unit : value

val environment_wrap : environment -> environment -> environment_wrap
val id_environment_wrap : environment -> environment_wrap
val e_var : ?loc:Location.t -> var_name -> type_expression -> expression
val ec_pair : expression -> expression -> expression_content
val e_application : ?loc:Location.t -> expression -> type_expression -> expression -> expression
