open Deku_concepts
open Deku_crypto

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
  tickets : Ticket.t list;
}
[@@deriving yojson]

type vm_client_message =
  | Control
  | Transaction of transaction
  | Set of set
  | Get_Initial_State
  | Set_Initial_State of State.t
  | Get of string
[@@deriving yojson]

type vm_server_message =
  | Init of set list
  | Stop
  | Set of set
  | Take_tickets of string
  | Deposit_tickets of { address : string; tickets : (Ticket.t * int64) list }
  | Error of string
[@@deriving yojson]
