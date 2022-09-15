open Deku_crypto
open Deku_concepts

module Consensus = struct
  open Packed

  let hash_packed_data data =
    data |> to_bytes |> Bytes.to_string |> BLAKE2b.hash

  let level level = nat (Level.to_n level)
  let hash hash = bytes (Bytes.of_string (BLAKE2b.to_raw hash))

  let hash_block ~block_level ~block_content =
    let packed = pair (level block_level) (hash block_content) in
    hash_packed_data packed
end
