open Helpers

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

module State = struct
  type t = Yojson.Safe.t String_map.t [@@deriving yojson]

  let get = String_map.find_opt

  let set = String_map.add

  let empty = String_map.empty
end
