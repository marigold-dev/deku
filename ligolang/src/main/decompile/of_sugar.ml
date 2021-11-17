open Ast_sugar
open Purification

let decompile (m : module_) : Ast_imperative.module_  =
  decompile_module m

let decompile_expression (e : expression) : Ast_imperative.expression  =
  decompile_expression e
