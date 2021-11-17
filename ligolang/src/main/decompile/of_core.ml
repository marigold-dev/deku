open Ast_core
open Desugaring

let decompile (m : module_) : Ast_sugar.module_  =
  decompile_module m

let decompile_expression (e : expression) : Ast_sugar.expression  =
  decompile_expression e
