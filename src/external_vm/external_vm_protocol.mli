open Deku_crypto
open Deku_stdlib

type set = { key : string; value : string } [@@deriving yojson]

module State : sig
  type t [@@deriving yojson]

  val get : string -> t -> string option
  val set : string -> string -> t -> t
  val empty : t
end

module Make (Ticket_id : sig
  type t [@@deriving yojson]
end) (Address : sig
  type t [@@deriving yojson]
end) : sig
  type transaction = {
    operation_raw_hash : string;
    source : Key_hash.t;
    operation : string;
    tickets : (Ticket_id.t * N.t) list;
    level : Deku_concepts.Level.t;
  }
  [@@deriving yojson]

  type vm_client_message =
    | Transaction of transaction
    | Noop_transaction
    | Set of set
    | Get_Initial_State
    | Give_Tickets of (Ticket_id.t * N.t) list
    | Set_Initial_State of State.t
    | Get of string
  [@@deriving yojson]

  type vm_server_message =
    | Init of set list
    | Stop
    | Set of set
    | Take_tickets of Address.t
    | Deposit_tickets of {
        address : Address.t;
        tickets : (Ticket_id.t * N.t) list;
      }
    | Error of string
  [@@deriving yojson]
end
