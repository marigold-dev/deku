open Deku_ledger

exception Not_a_state

type t [@@deriving ord, show]

val empty : t
val add_contract : t -> Contract_address.t -> State_entry.t -> t
val fetch_contract : t -> Contract_address.t -> State_entry.t

(* val to_json_api : t -> Data_encodiwng.Json.t *)
val encoding : t Data_encoding.t
val api_encoding : t Data_encoding.t
