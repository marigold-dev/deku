open Helpers;
open Protocol;

module Address_map = Map_with_yojson_make(Address);

[@deriving yojson]
type identity = {
  key: Address.key,
  t: Address.t,
  uri: Uri.t,
};

[@deriving yojson]
type t = {
  identity,
  pending_side_ops: list(Operation.Side_chain.Self_signed.t),
  pending_main_ops: list(Operation.Main_chain.t),
  pending_block_ops: Address_map.t(Multisig.t(Block.t)),
  protocol: Protocol.t,
};

let bootstrap = (~identity as (key, uri)) => {
  identity: {
    key,
    t: Address.of_key(key),
    uri,
  },
  pending_side_ops: [],
  pending_main_ops: [],
  pending_block_ops: Address_map.empty,
  protocol: Protocol.empty,
};
