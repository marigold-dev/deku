open Helpers;
open Crypto;

module Tezos_operation_set =
  Set.Make_with_yojson(Protocol_operation.Core_tezos);
module User_operation_set =
  Set.Make_with_yojson(Protocol_operation.Core_user);

[@deriving to_yojson]
type t = {
  core_state: Core.State.t,
  // TODO: more efficient lookup on included_operations
  // TODO: is this part of the protocol?
  included_tezos_operations: Tezos_operation_set.t,
  included_user_operations: User_operation_set.t,
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
  // local consensus(not hashed) ie any part of consensus that you
  // cannot agree with other nodes, because it depends on something local
  last_state_root_update: float,
  last_applied_block_timestamp: float,
  last_seen_membership_change_timestamp: float,
};

let hash = t => {
  // state_root_hash is part of last_block_hash
  let to_yojson = [%to_yojson:
    (
      Core.State.t,
      Tezos_operation_set.t,
      User_operation_set.t,
      Validators.t,
      BLAKE2B.t,
      int64,
      BLAKE2B.t,
      BLAKE2B.t,
    )
  ];
  let json =
    to_yojson((
      t.core_state,
      t.included_tezos_operations,
      t.included_user_operations,
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
