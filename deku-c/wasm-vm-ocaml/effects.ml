open Deku_ledger
open Deku_stdlib

type _ Effect.t +=
  | Deposit_tickets : Address.t * (Ticket_id.t * N.t) list -> unit Effect.t
  | Take_tickets : Address.t -> (Ticket_id.t * N.t) list Effect.t
  | Self_addr : Contract_address.t Effect.t
  | Sender_address : Address.t Effect.t
  | Source_address : Address.t Effect.t
  | Push_to_contract_stack : Wasm.Values.value -> unit Effect.t
  | Indirect_call : Int32.t * Wasm.Values.value list -> Int64.t Effect.t
  | Indirect_call_unit : Int32.t * Wasm.Values.value list -> unit Effect.t
  | Get_constant : Int64.t -> Value.t Effect.t
  | Read_ticket : int -> (Ticket_id.t * N.t * int) Effect.t
  | Split_ticket : int * (N.t * N.t) -> (int * int) option Effect.t
  | Join_ticket : int * int -> int option Effect.t
  | Mint_ticket : Ticket_id.t * N.t -> int Effect.t
  | Set_constants : (int * Value.t) array -> unit Effect.t
  | Set_instance : Wasm.Instance.module_inst -> unit Effect.t

let deposit_tickets (t, v) = Effect.perform (Deposit_tickets (t, v))
let take_tickets t = Effect.perform (Take_tickets t)
let self_addr () = Effect.perform Self_addr
let source_addr () = Effect.perform Source_address
let sender_addr () = Effect.perform Sender_address
let call_indirect t v = Effect.perform (Indirect_call (t, v))
let call_indirect_unit t v = Effect.perform (Indirect_call_unit (t, v))
let get_constant t = Effect.perform (Get_constant t)
let push_to_stack t = Effect.perform (Push_to_contract_stack t)
let read_ticket t = Effect.perform (Read_ticket t)
let split_ticket t amounts = Effect.perform (Split_ticket (t, amounts))
let join_tickets t t2 = Effect.perform (Join_ticket (t, t2))
let mint_ticket t amount = Effect.perform (Mint_ticket (t, amount))
let set_instance x = Effect.perform (Set_instance x)
let set_constants x = Effect.perform (Set_constants x)
