module CST = Cst.Pascaligo
module AST = Ast_imperative

module Compiler   = Compiler
module Decompiler = Decompiler
module Errors = Errors

let compile_module      = Compiler.compile_module
let compile_expression   = Compiler.compile_expression

let decompile_module    = Decompiler.decompile_module
let decompile_expression = Decompiler.decompile_expression
