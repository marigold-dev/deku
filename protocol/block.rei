open Crypto;
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
    author: Wallet.t,
    block_height: int64,
    main_chain_ops: list(Main_chain.t),
    side_chain_ops: list(Side_chain.t),
  };

let sign: (~key: Secret.t, t) => Protocol_signature.t;
let verify: (~signature: Protocol_signature.t, t) => bool;
let genesis: t;
let produce:
  (
    ~state: State.t,
    ~author: Wallet.t,
    ~main_chain_ops: list(Main_chain.t),
    ~side_chain_ops: list(Side_chain.t)
  ) =>
  t;
