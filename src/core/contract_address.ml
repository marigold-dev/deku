open Tezos
open Crypto

type t = Contract_hash.t [@@deriving yojson, eq, ord]

(* contract address is a hash of user_operation hash *)
let of_user_operation_hash (hash : BLAKE2B.t) =
  hash |> BLAKE2B.to_raw_string |> BLAKE2B_20.hash

let to_string = Contract_hash.to_string
let of_string = Contract_hash.of_string
