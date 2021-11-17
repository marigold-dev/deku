module Tree_abstraction : sig
  open Ast_imperative

  val pseudo_module_to_string : constant' -> string
  module type Constant = sig
    val constants      : string -> rich_constant option
    val constant_to_string      : rich_constant -> string
  end

  module Pascaligo : Constant

  module Cameligo : Constant

  module Reasonligo : Constant

  module Jsligo : Constant

end

module Stacking : sig
  include module type of Helpers.Stacking
  open Stage_common.Types
  val get_operators : Environment.Protocols.t -> constant' -> predicate option
end
