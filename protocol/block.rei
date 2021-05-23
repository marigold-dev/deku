open Helpers;
open Operation;

[@deriving (yojson, ord)]
type t =
  pri {
    hash: SHA256.hash,
    previous_hash: SHA256.hash,
    state_root_hash: SHA256.hash,
    author: Address.t,
    block_height: int64,
    main_chain_ops: list(Main_chain.t),
    side_chain_ops: list(Side_chain.Self_signed.t),
  };

let sign: (~key: Address.key, t) => Signature.t;
let verify: (~signature: Signature.t, t) => bool;

let genesis: t;
let produce:
  (
    ~state: State.t,
    ~author: Address.t,
    ~main_chain_ops: list(Main_chain.t),
    ~side_chain_ops: list(Side_chain.Self_signed.t)
  ) =>
  t;
