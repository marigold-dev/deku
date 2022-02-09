open Crypto;
open Core;

[@deriving (yojson, ord)]
type t =
  pri {
    hash: BLAKE2B.t,
    payload_hash: BLAKE2B.t,
    state_root_hash: BLAKE2B.t,
    handles_hash: BLAKE2B.t,
    validators_hash: BLAKE2B.t,
    previous_hash: BLAKE2B.t,
    author: Address.t,
    block_height: int64,
    operations: list(Protocol_operation.t),
  };

let sign: (~key: Secret.t, t) => Protocol_signature.t;
let verify: (~signature: Protocol_signature.t, t) => bool;
let genesis: t;
let produce:
  (
    ~state: Protocol_state.t,
    ~next_state_root_hash: option(BLAKE2B.t),
    ~author: Address.t,
    ~operations: list(Protocol_operation.t)
  ) =>
  t;
