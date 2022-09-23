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
  source : Key_hash.t;
  operation : string;
  tickets : Ticket.t list;
}
[@@deriving yojson]

type vm_client_message =
  | Control
  | Transaction of transaction
  | Noop_transaction
  | Set of set
  | Get_Initial_State
  | Set_Initial_State of State.t
  | Get of string
[@@deriving yojson]

type vm_server_message =
  | Init of set list
  | Stop
  | Set of set
  | Error of string
[@@deriving yojson]
