open Zinc_interpreter_intf

module Make (E : Executor) : sig
  module Types :
    Zinc_types.S
      with type Zinc.Key.t := E.Key.t
       and type Zinc.Address.t := E.Address.t
       and type Zinc.Contract.t := E.Contract.t
       and type Zinc.Chain_id.t := E.Chain_id.t
       and type Zinc.Hash.t := E.Hash.t

  module Interpreter : sig
    val initial_state :
      ?initial_stack:Types.Stack.t -> Types.Zinc.t -> Types.Interpreter_input.t

    val eval : 'a ->
           ('a -> E.Functions.t) -> Types.Interpreter_input.t -> Types.Interpreter_output.t
  end
end

module Dummy : sig
  module Types :
    Zinc_types.S
      with type Zinc.Key.t := string
       and type Zinc.Address.t := string
       and type Zinc.Contract.t := string * string option
       and type Zinc.Chain_id.t := string
       and type Zinc.Hash.t := string

  module Functions : sig
    type t = {get_contract_opt: (string -> (string * string option) option); chain_id: string}
  end 

  module Interpreter : sig
    val initial_state :
      ?initial_stack:Types.Stack.t -> Types.Zinc.t -> Types.Interpreter_input.t

    val eval : 'a ->
           ('a -> Functions.t) -> Types.Interpreter_input.t -> Types.Interpreter_output.t
  end
end
