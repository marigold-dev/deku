open Zinc_types
open Zinc_interpreter_intf

module Make (D : Domain_types) : sig
  module Ir :
    Zinc_instructions.Instructions.S
      with type Key.t = D.Key.t
       and type Address.t = D.Address.t
       and type Contract.t = D.Contract.t
       and type Chain_id.t = D.Chain_id.t
       and type Hash.t = D.Hash.t
       and type Key_hash.t = D.Key_hash.t

  module Types :
    Zinc_types.S
      with type Zinc.Key.t = D.Key.t
       and type Zinc.Address.t = D.Address.t
       and type Zinc.Contract.t = D.Contract.t
       and type Zinc.Chain_id.t = D.Chain_id.t
       and type Zinc.Hash.t = D.Hash.t
       and type Zinc.Key_hash.t = D.Key_hash.t

  module type Executor =
    Executor
      with type key := D.Key.t
      with type key_hash := D.Key_hash.t
       and type address := D.Address.t
       and type contract := D.Contract.t
       and type chain_id := D.Chain_id.t
       and type hash := D.Hash.t

  module Interpreter : sig
    val initial_state :
      ?initial_stack:Ir.Zt.Stack.t -> Ir.Zt.Zinc.t -> Ir.Zt.Interpreter_input.t

    val eval :
      (module Executor) ->
      Ir.Zt.Interpreter_input.t ->
      Ir.Zt.Interpreter_output.t
  end
end

module Dummy : sig
  module Ir :
    Zinc_instructions.Instructions.S
      with type Key.t = string
      with type Key_hash.t = string
       and type Address.t = string
       and type Contract.t = string * string option
       and type Chain_id.t = string
       and type Hash.t = string

  module Types :
    Zinc_types.S
      with type Zinc.Key.t = string
      with type Zinc.Key_hash.t = string
       and type Zinc.Address.t = string
       and type Zinc.Contract.t = string * string option
       and type Zinc.Chain_id.t = string
       and type Zinc.Hash.t = string

  module type Executor =
    Executor
      with type key := string
      with type key_hash := string
       and type address := string
       and type contract := string * string option
       and type chain_id := string
       and type hash := string

  module Interpreter : sig
    val initial_state :
      ?initial_stack:Ir.Zt.Stack.t -> Ir.Zt.Zinc.t -> Ir.Zt.Interpreter_input.t

    val eval :
      (module Executor) ->
      Ir.Zt.Interpreter_input.t ->
      Ir.Zt.Interpreter_output.t
  end
end
