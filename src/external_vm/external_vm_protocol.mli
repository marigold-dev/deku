type set = {
  key : string;
  value : Yojson.Safe.t;
}
[@@deriving yojson]

type vm_client_message =
  | Control
  | Source            of string
  | Tx_hash           of string
  | Operation         of Yojson.Safe.t
  | Get_Initial_State
  | Get               of Yojson.Safe.t
[@@deriving yojson]

type vm_server_message =
  | Init  of set list
  | Stop
  | Set   of set
  | Get   of string
  | Error of string
[@@deriving yojson]

module State : sig
  type t [@@deriving yojson]

  val get : string -> t -> Yojson.Safe.t option

  val set : string -> Yojson.Safe.t -> t -> t

  val empty : t
end
