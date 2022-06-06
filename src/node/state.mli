open Crypto
open Protocol

type identity = {
  secret : Secret.t;
  key : Key.t;
  t : Key_hash.t;
  uri : Uri.t;
}
[@@deriving yojson]

module Address_map : Map.S with type key = Key_hash.t

module Uri_map : Map.S with type key = Uri.t

module Operation_map : Map.S with type key = Protocol.Operation.t

type t = {
  identity : identity;
  validators_actor : Validator.Actor.t ;
  interop_context : Tezos_interop.t;
  data_folder : string;
  pending_operations : float Operation_map.t;
  block_pool : Block_pool.t;
  applied_blocks : Block.t list;
  protocol : Protocol.t;
  snapshots : Snapshots.t;
  uri_state : string Uri_map.t;
  validators_uri : Uri.t Address_map.t;
  recent_operation_receipts : Core_deku.State.receipt BLAKE2B.Map.t;
}

val make :
  identity:identity ->
  validators_actor:Validator.Actor.t ->
  interop_context:Tezos_interop.t ->
  data_folder:string ->
  initial_validators_uri:Uri.t Address_map.t ->
  t

val apply_block :
  t ->
  Block.t ->
  (t, [> `Invalid_block_when_applying | `Invalid_state_root_hash]) result

val load_snapshot :
  snapshot:Snapshots.snapshot ->
  additional_blocks:Block.t list ->
  last_block:Block.t ->
  last_block_signatures:Signature.t list ->
  t ->
  ( t,
    [> `Invalid_block_when_applying
    | `Invalid_state_root_hash
    | `Not_all_blocks_are_signed
    | `Snapshots_with_invalid_hash
    | `State_root_not_the_expected
    | `Invalid_snapshot_height ] )
  result
