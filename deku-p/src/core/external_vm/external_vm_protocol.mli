open Deku_crypto
open Deku_stdlib

module State : sig
  type t

  val encoding : t Data_encoding.t
  val get : string -> t -> string option
  val set : string -> string -> t -> t
  val empty : t
end

type set = { key : string; value : string }

type transaction = {
  operation_raw_hash : string;
  source : Key_hash.t;
  operation : string;
  tickets : (Deku_ledger.Ticket_id.t * N.t) list;
  level : Deku_concepts.Level.t;
}

type vm_client_message =
  | Transaction of transaction
  | Noop_transaction
  | Set of set
  | Get_Initial_State
  | Give_Tickets of (Deku_ledger.Ticket_id.t * N.t) list
  | Set_Initial_State of State.t
  | Get of string

val vm_client_message_encoding : vm_client_message Data_encoding.t

type vm_server_message =
  | Init of set list
  | Stop
  | Set of set
  | Take_tickets of Deku_ledger.Address.t
  | Deposit_tickets of {
      address : Deku_ledger.Address.t;
      tickets : (Deku_ledger.Ticket_id.t * N.t) list;
    }
  | Error of string

val vm_server_message_encoding : vm_server_message Data_encoding.t
