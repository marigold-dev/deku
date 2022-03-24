module Ast = Ast
module Gas = Gas

module Ir = struct
  type script = Ir.script
  type value = Ir.value

  let pp_value = Ir.pp_value
end

module Compiler = Compiler

module Interpreter = Interpreter
