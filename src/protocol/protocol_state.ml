open Helpers
open Crypto
module Tezos_operation_set = Set.Make_with_yojson (Protocol_operation.Core_tezos)
module User_operation_set = Set.Make_with_yojson (Protocol_operation.Core_user)
type t = {
  core_state : Core.State.t;
  included_tezos_operations : Tezos_operation_set.t;
  included_user_operations : User_operation_set.t;
  validators : Validators.t;
  validators_hash : BLAKE2B.t;
  block_height : int64;
  last_block_hash : BLAKE2B.t;
  state_root_hash : BLAKE2B.t;
  last_state_root_update : float;
  last_applied_block_timestamp : float;
  last_seen_membership_change_timestamp : float;
}
let hash t =
  let to_yojson =
    [%to_yojson:
      Core.State.t
      * Tezos_operation_set.t
      * User_operation_set.t
      * Validators.t
      * BLAKE2B.t
      * int64
      * BLAKE2B.t
      * BLAKE2B.t] in
  let json =
    to_yojson
      ( t.core_state,
        t.included_tezos_operations,
        t.included_user_operations,
        t.validators,
        t.validators_hash,
        t.block_height,
        t.last_block_hash,
        t.state_root_hash ) in
  let data = Yojson.Safe.to_string json in
  (BLAKE2B.hash data, data)
