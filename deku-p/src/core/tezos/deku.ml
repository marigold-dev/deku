open Deku_crypto
open Deku_concepts
open Deku_stdlib

module Consensus = struct
  open Pack

  let hash_packed_data data =
    data |> to_bytes |> Bytes.to_string |> BLAKE2b.hash

  let hash_validators validators =
    list (List.map key_hash validators) |> hash_packed_data

  let hash hash = bytes (BLAKE2b.to_raw hash |> Bytes.of_string)

  let hash_block ~block_level ~block_payload_hash ~state_root_hash
      ~withdrawal_handles_hash =
    let level = Level.to_n block_level |> N.to_z in
    pair
      (pair (int level) (hash block_payload_hash))
      (pair (hash withdrawal_handles_hash) (hash state_root_hash))
    |> hash_packed_data

  let hash_withdrawal_handle ~id ~owner ~amount ~ticketer ~data =
    pair
      (pair (pair (nat amount) (bytes data)) (pair (nat id) (address owner)))
      (address ticketer)
    |> hash_packed_data

  let hash_withdrawal_handle_deku ~id ~owner ~amount ~ticketer ~data =
    pair
      (pair (pair (nat amount) (bytes data)) (pair (nat id) (address owner)))
      (string_ ticketer)
    |> hash_packed_data
end
