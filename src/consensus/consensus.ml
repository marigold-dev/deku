include State
include Effect
include Load_snapshot

type t = state

let with_block effect block state =
  Dispatch.dispatch effect (Check_block { block }) state

let with_signature effect ~hash ~signature state =
  Dispatch.dispatch effect (Check_signature { hash; signature }) state

module Snapshots = Snapshots
module Trusted_validators_membership_change =
  Trusted_validators_membership_change
module Block_pool = Block_pool
module Signatures = Signatures
