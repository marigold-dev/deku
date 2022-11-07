open Deku_ledger

exception Not_a_state

type t [@@deriving ord, show, yojson]

val empty : t
val add_contract : t -> Contract_address.t -> State_entry.t -> t
val fetch_contract : t -> Contract_address.t -> State_entry.t
val to_json_api : t -> Yojson.Safe.t
