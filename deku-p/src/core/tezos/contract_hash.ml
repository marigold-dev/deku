open Deku_crypto
open BLAKE2b
open BLAKE2b_160

type t = BLAKE2b_160.t [@@deriving eq, ord, show]

include With_b58_and_encoding (struct
  let name = "Contract_hash"
  let prefix = Deku_repr.Prefix.contract_hash
end)
