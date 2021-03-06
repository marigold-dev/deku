open Crypto
open Protocol

type effect =
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
      snapshot_ref : Snapshots.snapshot_ref option;
    }
  | Persist_trusted_membership_change of {
      trusted_validator_membership_change :
        Trusted_validators_membership_change.Set.t;
    }

type t = effect
