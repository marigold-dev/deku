open Deku_repr
open Deku_crypto
open BLAKE2b

type block_hash = BLAKE2b.t
and t = block_hash [@@deriving eq, ord]

let of_blake2b hash = hash
let to_blake2b operation_hash = operation_hash

include With_b58 (struct
  let prefix = Prefix.deku_block_hash
end)

include With_yojson_of_b58 (struct
  type t = block_hash

  let of_b58 = of_b58
  let to_b58 = to_b58
end)

let hash ~block_level ~block_payload_hash ~state_root_hash
    ~withdrawal_handles_hash =
  let pre_hash =
    Deku_tezos.Deku.Consensus.hash_block ~block_level ~block_payload_hash
      ~state_root_hash ~withdrawal_handles_hash
  in
  pre_hash |> to_raw |> hash |> to_blake2b

module Map = Map
