
let decompile (m : Ast_typed.module_fully_typed) : Ast_core.module_  =
  Checking.untype_module_fully_typed m

let decompile_expression (e : Ast_typed.expression) : Ast_core.expression =
  Checking.untype_expression e
