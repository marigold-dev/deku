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
  let level_n = Deku_concepts.Level.to_n block_level in
  let pre_hash =
    Deku_tezos.Deku.Consensus.hash_block ~block_level ~block_payload_hash
      ~state_root_hash ~withdrawal_handles_hash
  in
  let final_hash = pre_hash |> to_raw |> hash |> to_blake2b in
  Format.printf
    "\n\n\
     level: %a\n\
     block_payload_hash:%a\n\
     state_root_hash:%a\n\
     withdraw_handles_hash:%a\n\
     pre_hash:%a\n\
     final_hash:%a\n\n\n\
    \   " Deku_stdlib.N.pp level_n BLAKE2b.pp block_payload_hash BLAKE2b.pp
    state_root_hash BLAKE2b.pp withdrawal_handles_hash BLAKE2b.pp pre_hash
    BLAKE2b.pp final_hash;
  final_hash

module Map = Map
