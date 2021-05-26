open Helpers;

module Operation_side_chain_set = Set_with_yojson_make(Operation.Side_chain);

type t = {
  // state machine data
  ledger: Ledger.t,
  // TODO: more efficient lookup on included_operations
  // TODO: is this part of the protocol?
  included_operations: Operation_side_chain_set.t,
  validators: Validators.t,
  // shared consensus
  block_height: int64,
  last_block_hash: SHA256.t,
  state_root_hash: SHA256.t,
  /* TODO: I really don't like this field here, as it means protocol
     state data will be different across nodes, of course we can just
     ignore like on the hashing function, but it still bothers me */
  // local consensus(not hashed)
  last_state_root_update: float,
};

let hash = t => {
  // state_root_hash is part of last_block_hash
  let to_yojson = [%to_yojson:
    (
      Ledger.t,
      Operation_side_chain_set.t,
      Validators.t,
      int64,
      SHA256.t,
      SHA256.t,
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
    ));
  SHA256.Magic.hash(Yojson.Safe.to_string(json));
};
