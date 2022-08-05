open Deku_crypto
include BLAKE2b.BLAKE2b_160

type contract_hash = t

include With_all_encodings (struct
  let name = "Contract_hash"
  let title = "A contract ID"
  let prefix = Deku_repr.Prefix.contract_hash
end)
