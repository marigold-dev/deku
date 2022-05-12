open Crypto

include BLAKE2B_20

(* TODO: STOP STRINGIFYING EVERYWHERE TO GET HASH WHEN HASH ALREADY EXISTS *)
let make owner ticket =
  BLAKE2B_20.hash_v [Address.to_string owner; Tezos.Ticket_id.to_string ticket]
let make_temp owner ticket amount =
  BLAKE2B_20.hash_v
    [
      Address.to_string owner;
      Tezos.Ticket_id.to_string ticket;
      Amount.to_int amount |> string_of_int;
    ]

let to_bytes t = BLAKE2B_20.to_raw_string t |> Bytes.of_string
let of_bytes t = Bytes.to_string t |> BLAKE2B_20.of_raw_string

module Map = Helpers.Map.Make_with_yojson (struct
  type nonrec t = t

  let to_yojson = to_yojson
  let of_yojson = of_yojson
  let compare = compare
end)
module Set = Helpers.Set.Make_with_yojson (struct
  type nonrec t = t

  let to_yojson = to_yojson
  let of_yojson = of_yojson
  let compare = compare
end)
