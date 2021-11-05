open Helpers;
open Crypto;
open Tezos;

module Address: {
  type t =
    | Implicit(Key_hash.t)
    | Originated({
        contract: Contract_hash.t,
        entrypoint: option(string),
      });

  let equal: (t, t) => bool;
  let to_string: t => string;
  let of_string: string => option(t);

  let to_yojson: t => Yojson.Safe.t;
  let of_yojson: Yojson.Safe.t => result(t, string);
};

module Ticket: {
  type t = {
    ticketer: Address.t,
    data: bytes,
  };
  let equal: (t, t) => bool;
  let to_string: t => string;
  let of_string: string => option(t);
};

module Operation_hash: {
  type t = BLAKE2B.t;
  let equal: (t, t) => bool;
  let to_string: t => string;
  let of_string: string => option(t);
};

module Pack: {
  type t;

  let int: Z.t => t;
  let bytes: bytes => t;
  let pair: (t, t) => t;
  let list: list(t) => t;
  let key: Key.t => t;
  let key_hash: Key_hash.t => t;
  let address: Address.t => t;

  let to_bytes: t => bytes;
};

module Context: {
  type t = {
    rpc_node: Uri.t,
    secret: Secret.t,
    consensus_contract: Address.t,
    required_confirmations: int,
  };
};

module Consensus: {
  let hash_validators: list(Key_hash.t) => BLAKE2B.t;
  let hash_block:
    (
      ~block_height: int64,
      ~block_payload_hash: BLAKE2B.t,
      ~state_root_hash: BLAKE2B.t,
      ~handles_hash: BLAKE2B.t,
      ~validators_hash: BLAKE2B.t
    ) =>
    BLAKE2B.t;
  let hash_withdraw_handle:
    (
      ~id: Z.t,
      ~owner: Address.t,
      ~amount: Z.t,
      ~ticketer: Address.t,
      ~data: bytes
    ) =>
    BLAKE2B.t;

  /** ~signatures should be in the same order as the old validators */
  let commit_state_hash:
    (
      ~context: Context.t,
      ~block_hash: BLAKE2B.t,
      ~block_height: int64,
      ~block_payload_hash: BLAKE2B.t,
      ~state_hash: BLAKE2B.t,
      ~handles_hash: BLAKE2B.t,
      ~validators: list(Key_hash.t),
      ~signatures: list((Key_hash.t, option(Signature.t)))
    ) =>
    Lwt.t(unit);

  type parameters =
    | Deposit({
        ticket: Ticket.t,
        // TODO: proper type for amounts
        amount: Z.t,
        destination: Address.t,
      })
    | Update_root_hash(BLAKE2B.t);
  type operation = {
    hash: Operation_hash.t,
    index: int,
    parameters,
  };
  let listen_operations:
    (~context: Context.t, ~on_operation: operation => unit) => unit;
  let fetch_validators:
    (~context: Context.t) => Lwt.t(result(list(Key_hash.t), string));
};

module Discovery: {let sign: (Secret.t, ~nonce: int64, Uri.t) => Signature.t;};
