type contract_state = {
  entrypoint: string option;
  originator: Address.Implicit.t;
  storage: Interpreter.Types.Stack_item.t;
  code: Interpreter.Types.Program.t
} [@@deriving yojson]

type t [@@deriving to_yojson]
val empty : t
val add : t -> Address.Originated.t -> contract_state -> t
val get : t -> Address.Originated.t -> contract_state option
val update_entry : t ->
    Address.Originated.t ->
    (contract_state option -> (contract_state * 'b, 'c) result) -> (t * 'b, 'c) result
val exists : t -> (Address.Originated.t -> contract_state -> bool) -> bool
val make_state : entrypoint:string option ->
    originator:Address.Implicit.t ->
    storage:Interpreter.Types.Stack_item.t ->
    code:Interpreter.Types.Program.t -> unit -> contract_state