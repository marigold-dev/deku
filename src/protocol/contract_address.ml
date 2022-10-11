open Deku_crypto
open Deku_repr
open BLAKE2b
open BLAKE2b_160

type t = BLAKE2b_160.t [@@deriving eq, ord]

let of_user_operation_hash t = BLAKE2b_256.to_raw t |> BLAKE2b_160.hash

include With_b58_and_encoding (struct
  let name = "Deku_contract_hash"
  let prefix = Deku_repr.Prefix.deku_contract_hash
end)

include With_yojson_of_b58 (struct
  type nonrec t = t

  let of_b58 = of_b58
  let to_b58 = to_b58
end)
