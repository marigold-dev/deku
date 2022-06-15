include State
include Effect
include Load_snapshot
include Consensus_utils

type t = state

type error =
  [ `Already_known_block
  | `Already_known_signature
  | `Invalid_block                   of string
  | `Invalid_block_when_applying
  | `Invalid_signature_for_this_hash
  | `Not_a_validator
  | `Failed_to_verify_payload
  | `Invalid_signature_author ]

let with_block effect block state =
  Dispatch.dispatch effect (Check_block { block }) state

let with_signature effect ~hash ~signature state =
  Dispatch.dispatch effect (Check_signature { hash; signature }) state

let with_timeout effect state = Dispatch.dispatch effect Can_produce_block state

let with_operation effect operation state =
  Dispatch.dispatch effect (Check_operation { operation }) state

let with_trusted_validators_membership_change effect ~payload ~signature state =
  Dispatch.dispatch effect (Check_validator_change { payload; signature }) state

module Snapshots = Snapshots
module Trusted_validators_membership_change =
  Trusted_validators_membership_change
module Block_pool = Block_pool
module Signatures = Signatures
