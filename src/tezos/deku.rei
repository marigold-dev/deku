open Crypto;

module Consensus: {
  let hash_validators: list(Key_hash.t) => BLAKE2B.t;
  let hash_block:
    (
      ~block_height: int64,
      ~block_payload_hash: BLAKE2B.t,
      ~state_root_hash: BLAKE2B.t,
      ~handles_hash: BLAKE2B.t,
      ~validators_hash: BLAKE2B.t
    ) =>
    BLAKE2B.t;
  let hash_withdraw_handle:
    (
      ~id: Z.t,
      ~owner: Address.t,
      ~amount: Z.t,
      ~ticketer: Address.t,
      ~data: bytes
    ) =>
    BLAKE2B.t;
};
