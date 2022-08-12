open Deku_crypto
open Deku_repr
open BLAKE2b

type t = BLAKE2b_160.t [@@deriving eq, ord]

let of_user_operation_hash t = Operation_hash.to_b58 t |> BLAKE2b_160.hash

include With_b58_and_yojson (struct
  type nonrec t = t

  let prefix = Base58.Prefix.deku_contract_hash
  let to_raw = BLAKE2b_160.to_raw
  let of_raw = BLAKE2b_160.of_raw
end)
