[@@@warning "-45"]


module CST = Cst.Jsligo
module AST = Ast_imperative
module Errors = Errors

val compile_expression : raise:Errors.abs_error Trace.raise -> CST.expr -> AST.expr
val compile_module : raise:Errors.abs_error Trace.raise -> CST.ast -> AST.module_

val decompile_expression: AST.expr -> CST.expr list
val decompile_module: AST.module_ -> CST.ast
