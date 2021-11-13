module Errors = Errors
module Compiler   = Compiler
module Decompiler = Decompiler

let compile_module    = Compiler.compile_module
let compile_expression = Compiler.compile_expression


let decompile_module    = Decompiler.decompile_module
let decompile_expression = Decompiler.decompile_expression
