

open Tezos

type unparsed = string * string option [@@deriving yojson]

(** Address + entrypoint, @TODO: decide on entrypoint *)
type t = Contract_hash.t * string option [@@deriving yojson]

