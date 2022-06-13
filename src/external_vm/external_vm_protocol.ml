open Helpers
open Crypto

module State = struct
  type t = Yojson.Safe.t String_map.t [@@deriving yojson]

  let get = String_map.find_opt

  let set = String_map.add

  let empty = String_map.empty
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
