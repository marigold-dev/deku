open Helpers;
open Operation;

[@deriving (yojson, ord)]
type t =
  pri {
    hash: SHA256.t,
    payload_hash: SHA256.t,
    state_root_hash: SHA256.t,
    previous_hash: SHA256.t,
    author: Address.t,
    block_height: int64,
    main_chain_ops: list(Main_chain.t),
    side_chain_ops: list(Side_chain.Self_signed.t),
  };

let sign: (~key: Address.key, t) => Signature.t;
let verify: (~signature: Signature.t, t) => bool;
let verify_hash: (~signature: Signature.t, SHA256.t) => bool;
let genesis: t;
let produce:
  (
    ~state: State.t,
    ~author: Address.t,
    ~main_chain_ops: list(Main_chain.t),
    ~side_chain_ops: list(Side_chain.Self_signed.t)
  ) =>
  t;
