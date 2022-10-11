open Deku_crypto
open Deku_repr
open BLAKE2b
open BLAKE2b_160

type t = BLAKE2b_160.t [@@deriving eq, ord, show]

include With_b58_and_encoding (struct
  let name = "Contract_hash"
  let prefix = Deku_repr.Prefix.contract_hash
end)

include With_yojson_of_b58 (struct
  type nonrec t = t

  let of_b58 = of_b58
  let to_b58 = to_b58
end)
