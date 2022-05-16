open Helpers
open Crypto
open Protocol
open Core

module Signature_spec = struct
  type request = {
    hash : BLAKE2B.t;
    signature : Signature.t;
  }
  [@@deriving yojson]
  type response = unit [@@deriving yojson]
  let path = "/append-signature"
end

module Block_and_signature_spec = struct
  type request = {
    block : Block.t;
    signature : Signature.t;
  }
  [@@deriving yojson]
  type response = unit [@@deriving yojson]
  let path = "/append-block-and-signature"
end

module Block_by_hash_spec = struct
  type request = { hash : BLAKE2B.t } [@@deriving yojson]
  type response = Block.t option [@@deriving yojson]
  let path = "/block-by-hash"
end

module Block_level = struct
  type request = unit [@@deriving yojson]
  type response = { level : int64 } [@@deriving yojson]
  let path = "/block-level"
end

module Protocol_snapshot = struct
  type request = unit [@@deriving yojson]

  type snapshot = {
    hash : BLAKE2B.t;
    data : string;
  }
  [@@deriving yojson]
  type response = {
    snapshot : snapshot;
    additional_blocks : Block.t list;
    last_block : Block.t;
    last_block_signatures : Signature.t list;
  }
  [@@deriving yojson]
  let path = "/protocol-snapshot"
end

module Request_nonce = struct
  type request = { uri : Uri.t } [@@deriving yojson]
  type response = { nonce : BLAKE2B.t } [@@deriving yojson]
  let path = "/request-nonce"
end

module Register_uri = struct
  type request = {
    uri : Uri.t;
    signature : Signature.t;
  }
  [@@deriving yojson]
  type response = unit [@@deriving yojson]
  let path = "/register-uri"
end

module User_operation_gossip = struct
  type request = { user_operation : Protocol.Operation.Core_user.t }
  [@@deriving yojson]
  type response = unit [@@deriving yojson]
  let path = "/user-operation-gossip"
end

module User_operations_gossip = struct
  type request = { user_operations : Protocol.Operation.Core_user.t list }
  [@@deriving yojson]
  type response = unit [@@deriving yojson]
  let path = "/user-operations-gossip"
end

module Consensus_operation_gossip = struct
  type request = {
    consensus_operation : Protocol.Operation.Consensus.t;
    signature : Crypto.Signature.t;
  }
  [@@deriving yojson]
  type response = unit [@@deriving yojson]
  let path = "/consensus-operation-gossip"
end

module Withdraw_proof = struct
  type request = { operation_hash : BLAKE2B.t } [@@deriving yojson]
  type response =
    | Ok                          of {
        withdrawal_handles_hash : BLAKE2B.t;
        withdrawal_handle : Ledger.Withdrawal_handle.t;
        proof : (BLAKE2B.t * BLAKE2B.t) list;
      }
    | Unknown_operation
    | Operation_is_not_a_withdraw
  [@@deriving yojson]
  let path = "/withdraw-proof"
end

module Ticket_balance = struct
  type request = {
    address : Key_hash.t;
    ticket : Ticket_id.t;
  }
  [@@deriving yojson]
  type response = { amount : Amount.t } [@@deriving yojson]
  let path = "/ticket-balance"
end

module Trusted_validators_membership_change = struct
  type action =
    | Add
    | Remove
  [@@deriving yojson]
  type payload = {
    action : action;
    address : Key_hash.t;
  }
  [@@deriving yojson]
  type request = {
    signature : Signature.t;
    payload : payload;
  }
  [@@deriving yojson]
  type response = unit [@@deriving yojson]
  let path = "/trusted-validators-membership"
end
