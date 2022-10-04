open Deku_crypto
open BLAKE2b

type t = BLAKE2b_160.t [@@deriving eq, ord, show]

let of_user_operation_hash t = BLAKE2b_256.to_raw t |> BLAKE2b_160.hash

include Encoding_helpers.Make_b58 (struct
  type nonrec t = t

  let name = "Deku_contract_hash"
  let title = "A Deku contract ID"
  let size = BLAKE2b_160.digest_size
  let prefix = Deku_repr.Prefix.deku_contract_hash
  let to_raw = BLAKE2b_160.to_raw
  let of_raw = BLAKE2b_160.of_raw
end)

include Deku_repr.With_b58_and_yojson (struct
  type nonrec t = t

  let prefix = Deku_repr.Prefix.deku_contract_hash
  let to_raw = BLAKE2b_160.to_raw
  let of_raw string = BLAKE2b_160.of_raw string
end)
