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

val deposit_tickets : Address.t * (Ticket_id.t * N.t) list -> unit
val take_tickets : Address.address -> (Ticket_id.t * N.t) list
val self_addr : unit -> Contract_address.t
val sender_addr : unit -> Address.t
val source_addr : unit -> Address.t
val call_indirect : int32 -> Wasm.Values.value list -> int64
val call_indirect_unit : int32 -> Wasm.Values.value list -> unit
val get_constant : int64 -> Value.t
val push_to_stack : Wasm.Values.value -> unit
val read_ticket : int -> Ticket_id.t * N.t * int
val join_tickets : int -> int -> int option
val split_ticket : int -> N.t * N.t -> (int * int) option
val mint_ticket : Ticket_id.t -> N.t -> int
val set_constants : (int * Value.t) array -> unit
val set_instance : Wasm.Instance.module_inst -> unit
