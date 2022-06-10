open Crypto
include BLAKE2B_20

let make owner ticket amount =
  BLAKE2B_20.hash_v
    [
      Address.to_string owner;
      Tezos.Ticket_id.to_string ticket;
      Int.to_string (Amount.to_int amount);
    ]

let to_bytes t = to_string t |> Bytes.of_string

let of_bytes t = Bytes.to_string t |> of_string

module Table = Hashtbl.Make (struct
  type nonrec t = t

  let hash a = Hashtbl.hash a

  let equal a b = equal a b
end)
