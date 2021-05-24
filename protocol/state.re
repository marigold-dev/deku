open Helpers;

module Operation_side_chain_set = Set_with_yojson_make(Operation.Side_chain);
[@deriving yojson]
type t = {
  ledger: Ledger.t,
  // TODO: more efficient lookup on included_operations
  // TODO: is this part of the protocol?
  included_operations: Operation_side_chain_set.t,
  validators: Validators.t,
  block_height: int64,
  last_block_hash: SHA256.hash,
  /* TODO: I really don't like this field here, as it means protocol
     state data will be different across nodes, of course we can just
     ignore like on the hashing function, but it still bothers me */
  last_state_root_update: float,
  state_root_hash: SHA256.hash,
  pending_state_root_hash: SHA256.hash,
};

let hash = t => {
  // state_root_hash is part of last_block_hash
  let to_yojson = [%to_yojson:
    (
      Ledger.t,
      Operation_side_chain_set.t,
      Validators.t,
      int64,
      SHA256.hash,
      SHA256.hash,
      SHA256.hash,
    )
  ];
  let json =
    to_yojson((
      t.ledger,
      t.included_operations,
      t.validators,
      t.block_height,
      t.last_block_hash,
      t.state_root_hash,
      t.pending_state_root_hash,
    ));
  SHA256.Magic.hash(Yojson.Safe.to_string(json));
};
