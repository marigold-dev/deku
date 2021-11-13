open Errors
open Trace

module I = Ast_core
module O = Ast_typed

module Errors = Errors

type environment = O.Environment.t

val type_module      : raise:typer_error raise -> test:bool -> init_env:environment -> protocol_version:Environment.Protocols.t -> I.module_ -> environment * O.module_fully_typed
val type_declaration : raise:typer_error raise -> protocol_version:Environment.Protocols.t -> test:bool -> environment -> I.declaration Location.wrap -> environment * O.declaration Location.wrap
val evaluate_type    : raise:typer_error raise -> ?other_module:bool -> environment -> I.type_expression -> O.type_expression
val type_expression  : raise:typer_error raise -> test:bool -> protocol_version:Environment.Protocols.t -> environment -> ?tv_opt:O.type_expression -> I.expression -> O.environment * O.expression
val type_constant    : raise:typer_error raise -> test:bool -> protocol_version:Environment.Protocols.t -> I.constant' -> Location.t -> O.type_expression list -> O.type_expression option -> O.constant' * O.type_expression
(*
val untype_type_value : O.type_value -> (I.type_expression) result
val untype_literal : O.literal -> I.literal result
*)
val untype_expression : O.expression -> I.expression
(*
val untype_matching : ('o -> 'i result) -> 'o O.matching -> ('i I.matching) result
*)

val untype_module_fully_typed : O.module_fully_typed -> I.module_

module Environment = O.Environment
val assert_type_expression_eq : raise:typer_error raise -> Location.t -> O.type_expression * O.type_expression -> unit
val decompile_env : O.Environment.t -> I.Environment.t
