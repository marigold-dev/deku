open Deku_crypto
open Deku_tezos

module State : sig
  type t [@@deriving yojson]

  val get : string -> t -> string option
  val set : string -> string -> t -> t
  val empty : t
end

type set = { key : string; value : string } [@@deriving yojson]

type transaction = {
  operation_hash_raw : string;
  source : Key_hash.t;
  operation : string;
  tickets : (Ticket_id.t * int64) list;
}
[@@deriving yojson]

type vm_client_message =
  | Transaction of transaction
  | Noop_transaction
  | Set of set
  | Get_Initial_State
  | Give_Tickets of (Ticket_id.t * int64) list
  | Set_Initial_State of State.t
  | Get of string
[@@deriving yojson]

type vm_server_message =
  | Init of set list
  | Stop
  | Set of set
  | Take_tickets of string
  | Deposit_tickets of {
      address : string;
      tickets : (Ticket_id.t * int64) list;
    }
  | Error of string
[@@deriving yojson]
