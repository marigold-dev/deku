include State
include Effect
include Load_snapshot

type t = state

type 'a error =
  [> `Already_known_block
  | `Already_known_signature
  | `Invalid_block                   of string
  | `Invalid_block_when_applying
  | `Invalid_signature_for_this_hash
  | `Not_a_validator ]
  as
  'a

let with_block effect block state =
  Dispatch.dispatch effect (Check_block { block }) state

let with_signature effect ~hash ~signature state =
  Dispatch.dispatch effect (Check_signature { hash; signature }) state

module Snapshots = Snapshots
module Trusted_validators_membership_change =
  Trusted_validators_membership_change
module Block_pool = Block_pool
module Signatures = Signatures
