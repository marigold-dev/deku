(* TODO: move this with the AST, probably? *)
module Axioms = Axioms
module Typelang = Typelang

module Type_variable = struct type t = Ast_typed.Types.type_variable end

module Opaque_type_variable = struct
  module Types = Ast_typed.Types
  module Compare = struct
    include Ast_typed.Compare
  end
  module PP = struct
    include Ast_typed.PP
  end
  module Yojson = Ast_typed.Yojson
  module Solver_types = Solver_types
  module Misc = Ast_typed.Misc
  module Reasons = Ast_typed.Reasons
  module Axioms = Axioms
  module Core   = Typesystem.Core
  module Typelang = Typelang
end
module Check : Type_variable_abstraction.TYPE_VARIABLE_ABSTRACTION(Type_variable).S = Opaque_type_variable
