open Deku_crypto
open Deku_stdlib
open Tezos_pack

module Consensus = struct
  let hash_packed_data data =
    data |> to_bytes |> Bytes.to_string |> BLAKE2b.hash

  let hash_validators validators =
    list (List.map key_hash validators) |> hash_packed_data

  let hash hash = bytes (BLAKE2b.to_raw hash |> Bytes.of_string)

  let hash_block ~block_level ~block_payload_hash ~state_root_hash
      ~withdrawal_handles_hash =
    pair
      (pair (nat block_level) (hash block_payload_hash))
      (pair (hash withdrawal_handles_hash) (hash state_root_hash))
    |> hash_packed_data

  let hash_withdrawal_handle ~id ~owner ~amount ~ticket_id =
    let open Tezos_ticket_id in
    let (Tezos_ticket_id { ticketer; data }) = ticket_id in
    let id = Z.of_int id in
    let owner = Tezos_address.make ~contract:owner ~entrypoint:"" in
    let ticketer = Tezos_address.make ~contract:ticketer ~entrypoint:"" in
    pair
      (pair (pair (nat amount) (bytes data)) (pair (int id) (address owner)))
      (address ticketer)
    |> hash_packed_data
end

module Ticket_data = struct
  let encode ~ticketer ~data = to_bytes (pair (bytes ticketer) (bytes data))
  let decode = unpair_of_bytes
end
