open Crypto

module Consensus = struct
  open Pack

  let hash_packed_data data =
    data |> to_bytes |> Bytes.to_string |> BLAKE2B.hash

  let hash_validators validators =
    list (List.map key_hash validators) |> hash_packed_data

  let hash hash = bytes (BLAKE2B.to_raw_string hash |> Bytes.of_string)

  let hash_block ~block_height ~block_payload_hash ~state_root_hash
      ~withdrawal_handles_hash ~validators_hash =
    pair
      (pair
         (pair (int (Z.of_int64 block_height)) (hash block_payload_hash))
         (pair (hash withdrawal_handles_hash) (hash state_root_hash)))
      (hash validators_hash)
    |> hash_packed_data

  let hash_withdraw_handle ~id ~owner ~amount ~ticketer ~data =
    pair
      (pair (pair (nat amount) (bytes data)) (pair (nat id) (address owner)))
      (address ticketer)
    |> hash_packed_data
end
