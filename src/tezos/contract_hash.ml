open Deku_crypto
open Deku_repr
open BLAKE2b

type t = BLAKE2b_160.t [@@deriving eq, ord, show]

include With_encoding (struct
  type nonrec t = t

  let name = "Contract_hash"
  let title = "A contract ID"
  let size = BLAKE2b_160.digest_size
  let prefix = Deku_repr.Prefix.contract_hash
  let to_raw = BLAKE2b_160.to_raw
  let of_raw = BLAKE2b_160.of_raw
end)

include With_b58_and_yojson (struct
  type nonrec t = t

  let prefix = Deku_repr.Prefix.contract_hash
  let to_raw = BLAKE2b_160.to_raw
  let of_raw = BLAKE2b_160.of_raw
end)
