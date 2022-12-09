open Deku_repr
open Deku_crypto
open BLAKE2b

type block_hash = BLAKE2b.t
and t = block_hash [@@deriving eq, ord]

let of_blake2b hash = hash
let to_blake2b operation_hash = operation_hash

include With_b58_and_encoding (struct
  let name = "Deku_consensus.Block_hash"
  let prefix = Prefix.deku_block_hash
end)

let hash ~block_level ~block_payload_hash ~state_root_hash
    ~withdrawal_handles_hash =
  let pre_hash =
    Deku_tezos.Deku.Consensus.hash_block ~block_level ~block_payload_hash
      ~state_root_hash ~withdrawal_handles_hash
  in
  pre_hash |> to_raw |> hash |> to_blake2b

module Set = Set
module Map = Map

let show = to_b58
let pp fmt t = Format.pp_print_string fmt (to_b58 t)
