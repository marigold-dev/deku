open Helpers;
open Operation;

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
    main_chain_ops: list(Main_chain.t),
    side_chain_ops: list(Side_chain.t),
  };

let sign: (~key: Address.key, t) => Signature.t;
let verify: (~signature: Signature.t, t) => bool;
let genesis: t;

type next_hashes = {
  state_root: BLAKE2B.t,
  validators: BLAKE2B.t
}

let produce:
  (
    ~state: State.t,
    ~next_hashes: option(next_hashes),
    ~author: Address.t,
    ~main_chain_ops: list(Main_chain.t),
    ~side_chain_ops: list(Side_chain.t)
  ) =>
  t;
