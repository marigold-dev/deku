open Helpers

type set = {
  key : string;
  value : Yojson.Safe.t;
}
[@@deriving yojson]

type vm_message =
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
