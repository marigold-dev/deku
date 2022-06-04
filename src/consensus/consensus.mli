open Crypto
open Protocol

module Operation_map : Map.S with type key = Protocol.Operation.t

type identity = {
  secret : Secret.t;
  key : Key.t;
  t : Key_hash.t;
  uri : Uri.t;
}

type state = {
  (* TODO: duplicated on Node.State.t *)
  identity : identity;
  trusted_validator_membership_change :
    Trusted_validators_membership_change.Set.t;
  pending_operations : float Operation_map.t;
  block_pool : Block_pool.t;
  protocol : Protocol.t;
  snapshots : Snapshots.t;
}

type t = state

val make :
  identity:identity ->
  trusted_validator_membership_change:Trusted_validators_membership_change.Set.t ->
  t

type effect = private
  | Request_block                     of { hash : BLAKE2B.t }
  | Request_previous_blocks           of { block : Block.t }
  | Broadcast_block                   of { block : Block.t }
  | Broadcast_signature               of {
      hash : BLAKE2B.t;
      signature : Signature.t;
    }
  | Broadcast_user_operation          of {
      user_operation : Operation.Core_user.t;
    }
  | Applied_block                     of {
      block : Block.t;
      receipts : (BLAKE2B.t * Core_deku.State.receipt) list;
      prev_protocol : Protocol.t;
      self_signed : Signatures.t option;
    }
  | Persist_trusted_membership_change of {
      trusted_validator_membership_change :
        Trusted_validators_membership_change.Set.t;
    }

type error =
  [ `Already_known_block
  | `Already_known_signature
  | `Invalid_block                   of string
  | `Invalid_block_when_applying
  | `Invalid_signature_for_this_hash
  | `Not_a_validator
  | `Failed_to_verify_payload
  | `Invalid_signature_author ]

val with_block : (effect -> t -> unit) -> Block.t -> t -> t * [> error] list

val with_signature :
  (effect -> t -> unit) ->
  hash:BLAKE2B.t ->
  signature:Signature.t ->
  t ->
  t * [> error] list

val with_timeout : (effect -> t -> unit) -> t -> t * [> error] list

val with_operation :
  (effect -> t -> unit) -> Operation.t -> t -> t * [> error] list

val with_trusted_validators_membership_change :
  (effect -> t -> unit) ->
  payload:Network.Trusted_validators_membership_change.payload ->
  signature:Signature.t ->
  t ->
  t * [> error] list

val load_snapshot :
  snapshot:Snapshots.snapshot ->
  additional_blocks:Block.t list ->
  last_block:Block.t ->
  last_block_signatures:Signature.t list ->
  t ->
  ( t,
    [> error
    | `Invalid_snapshot_height
    | `Not_all_blocks_are_signed
    | `Snapshots_with_invalid_hash
    | `State_root_not_the_expected ] )
  result

val new_snapshot_ref : t -> Protocol.t -> t * Snapshots.snapshot_ref

val block_matches_current_state_root_hash : state -> Block.t -> bool

val block_matches_next_state_root_hash : state -> Block.t -> bool

module Snapshots = Snapshots
module Trusted_validators_membership_change =
  Trusted_validators_membership_change
module Block_pool = Block_pool
module Signatures = Signatures
