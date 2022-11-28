open Deku_crypto
open BLAKE2b
open BLAKE2b_160

type tezos_contract_hash = BLAKE2b_160.t
and t = tezos_contract_hash [@@deriving eq, ord, show]

include With_b58_and_encoding_and_yojson (struct
  let name = "Contract_hash"
  let prefix = Deku_repr.Prefix.contract_hash
end)
