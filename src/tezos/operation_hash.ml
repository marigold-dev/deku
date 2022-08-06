open Deku_repr
open Deku_crypto
open BLAKE2b

type t = BLAKE2b.t [@@deriving eq, ord]

include With_b58 (struct
  let prefix = Prefix.deku_block_hash
end)
