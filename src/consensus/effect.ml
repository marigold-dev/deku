open Crypto
open Protocol

type effect =
  | Request_block           of { hash : BLAKE2B.t }
  | Request_previous_blocks of { block : Block.t }
  | Broadcast_block         of { block : Block.t }
  | Broadcast_signature     of {
      hash : BLAKE2B.t;
      signature : Signature.t;
    }
  | Applied_block           of {
      block : Block.t;
      receipts : (BLAKE2B.t * Core_deku.State.receipt) list;
      trusted_validator_membership_change :
        Trusted_validators_membership_change.Set.t;
    }

type t = effect
