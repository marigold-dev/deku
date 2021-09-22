open Helpers;

module Operation_side_chain_set = Set_with_yojson_make(Operation.Side_chain);
module Operation_main_chain_set = Set_with_yojson_make(Operation.Main_chain);

type t = {
  // state machine data
  ledger: Ledger.t,
  // TODO: more efficient lookup on included_operations
  // TODO: is this part of the protocol?
  included_operations: Operation_side_chain_set.t,
  included_main_operations: Operation_main_chain_set.t,
  // consensus
  validators: Validators.t,
  validators_hash: BLAKE2B.t,
  // shared consensus
  block_height: int64,
  last_block_hash: BLAKE2B.t,
  state_root_hash: BLAKE2B.t,
  /* TODO: I really don't like this field here, as it means protocol
     state data will be different across nodes, of course we can just
     ignore like on the hashing function, but it still bothers me */
  // local consensus(not hashed)
  last_state_root_update: float,
  last_applied_block_timestamp: float,
};

let hash = t => {
  // state_root_hash is part of last_block_hash
  let to_yojson = [%to_yojson:
    (
      Ledger.t,
      Operation_side_chain_set.t,
      Operation_main_chain_set.t,
      Validators.t,
      BLAKE2B.t,
      int64,
      BLAKE2B.t,
      BLAKE2B.t,
    )
  ];
  let json =
    to_yojson((
      t.ledger,
      t.included_operations,
      t.included_main_operations,
      t.validators,
      t.validators_hash,
      t.block_height,
      t.last_block_hash,
      t.state_root_hash,
    ));

  let data = Yojson.Safe.to_string(json);
  // TODO: better way to carry hash and content together
  (BLAKE2B.hash(data), data);
};
