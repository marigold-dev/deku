open Crypto

module State : sig
  type t [@@deriving yojson]

  val get : string -> t -> Yojson.Safe.t option

  val set : string -> Yojson.Safe.t -> t -> t

  val empty : t

  val equal : t -> t -> bool
end

type set = {
  key : string;
  value : Yojson.Safe.t;
}
[@@deriving yojson]

type transaction = {
  source : Key_hash.t;
  tx_hash : BLAKE2B.t;
  op_hash : BLAKE2B.t;
  operation : Yojson.Safe.t;
}
[@@deriving yojson]

type vm_client_message =
  | Control
  | Transaction       of transaction
  | Set               of set
  | Get_Initial_State
  | Set_Initial_State of State.t
  | Get               of Yojson.Safe.t
[@@deriving yojson]

type vm_server_message =
  | Init  of set list
  | Stop
  | Set   of set
  | Error of string
[@@deriving yojson]
