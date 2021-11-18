open Trace

open Typer_common.Errors
module Errors = Typer_common.Errors
module I = Ast_core
module O = Ast_core
module O' = Solver              (* needs an instance of the solver now *)

module Environment = O.Environment

module Solver = Solver

type environment = Environment.t

val evaluate_type         : raise:typer_error raise -> environment -> I.type_expression -> O.type_expression
val type_expression       : raise:typer_error raise -> ?tv_opt:O.type_expression -> environment -> typer_error O'.typer_state -> I.expression -> environment * typer_error O'.typer_state * O.expression * O.type_expression
val type_expression_subst : raise:typer_error raise -> environment -> typer_error O'.typer_state -> ?tv_opt:O.type_expression -> I.expression -> environment * O.expression * O.type_expression * typer_error O'.typer_state
val type_declaration      : raise:typer_error raise -> environment -> typer_error O'.typer_state -> I.declaration Location.wrap -> environment * typer_error O'.typer_state * O.declaration Location.wrap * O.type_expression
val type_module           : raise:typer_error raise -> init_env:environment -> I.module_ -> environment * O.module_ * O.type_expression * typer_error O'.typer_state
