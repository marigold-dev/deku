open Crypto
open Core

include BLAKE2B_20

(* TODO: STOP STRINGIFYING EVERYWHERE TO GET HASH WHEN HASH ALREADY EXISTS *)
let make owner ticket amount =
  BLAKE2B_20.hash_v
    [
      Address.to_string owner;
      Tezos.Ticket_id.to_string ticket;
      Int.to_string (Amount.to_int amount);
    ]

let to_bytes t = to_string t |> Bytes.of_string
let of_bytes t = Bytes.to_string t |> of_string

module Map = Helpers.Map.Make_with_yojson (BLAKE2B_20)
module Set = Helpers.Set.Make_with_yojson (BLAKE2B_20)
