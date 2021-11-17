open Trace
open Errors

module AST = Ast_typed
module Append_tree = Tree.Append
module Errors = Errors
open Mini_c

val compile_expression : raise:spilling_error raise -> ?module_env: AST.type_expression Compiler.SMap.t -> AST.expression -> Mini_c.expression
val compile_module     : raise:spilling_error raise -> ?module_env: AST.type_expression Compiler.SMap.t -> AST.module_fully_typed -> program * AST.type_expression Compiler.SMap.t
val compile_type : raise:spilling_error raise -> AST.type_expression -> Mini_c.type_expression
val decompile : raise:spilling_error raise -> value -> AST.type_expression -> AST.expression
