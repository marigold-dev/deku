open Helpers
open Crypto
open Protocol_operation
module Tezos_operation_set = Set.Make_with_yojson (Protocol_operation.Core_tezos)

module Included_user_operation_set : sig
  type t [@@deriving yojson]

  val empty : t

  val add : Core_user.t -> t -> t

  val mem : Core_user.t -> t -> bool

  val crop : block_height:int64 -> t -> t
end = struct
  (* TODO: move this somewhere else *)
  (* TODO: idea here is that because of that we don't need to hold the
      full operation on the protocol only it's hash temporarily*)
  type t = int64 BLAKE2B.Map.t [@@deriving yojson]

  let empty = BLAKE2B.Map.empty

  let add user_operation t =
    BLAKE2B.Map.add user_operation.Core_user.hash user_operation.block_height t

  let mem user_operation t = BLAKE2B.Map.mem user_operation.Core_user.hash t

  let crop ~block_height t =
    BLAKE2B.Map.filter
      (fun _hash op_block_height -> op_block_height <= block_height)
      t
end

type t = {
  core_state : Core_deku.State.t;
  included_tezos_operations : Tezos_operation_set.t;
  included_user_operations : Included_user_operation_set.t;
  validators : Validators.t;
  validators_hash : BLAKE2B.t;
  block_height : int64;
  last_block_hash : BLAKE2B.t;
  state_root_hash : BLAKE2B.t;
  last_state_root_update : float;
  last_applied_block_timestamp : float;
  last_seen_membership_change_timestamp : float;
}
[@@deriving yojson]

let hash t =
  let to_yojson =
    [%to_yojson:
      Core_deku.State.t
      * Tezos_operation_set.t
      * Included_user_operation_set.t
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
