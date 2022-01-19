open Helpers;
open Crypto;
open Tezos;

module Context: {
  type t = {
    rpc_node: Uri.t,
    secret: Secret.t,
    consensus_contract: Address.t,
    required_confirmations: int,
  };
  let to_yojson: t => Yojson.Safe.t;
};

module Consensus: {
  /** ~signatures should be in the same order as the old validators */
  let commit_state_hash:
    (
      ~context: Context.t,
      ~block_height: int64,
      ~block_payload_hash: BLAKE2B.t,
      ~state_hash: BLAKE2B.t,
      ~handles_hash: BLAKE2B.t,
      ~validators: list(Key_hash.t),
      ~signatures: list(option((Key.t, Signature.t)))
    ) =>
    Lwt.t(unit);

  type transaction =
    | Deposit({
        ticket: Ticket_id.t,
        // TODO: proper type for amounts
        amount: Z.t,
        destination: Address.t,
      })
    | Update_root_hash(BLAKE2B.t);
  type operation = {
    hash: Operation_hash.t,
    transactions: list(transaction),
  };
  let listen_operations:
    (~context: Context.t, ~on_operation: operation => unit) => unit;
  let fetch_validators:
    (~context: Context.t) => Lwt.t(result(list(Key_hash.t), string));
};

module Discovery: {let sign: (Secret.t, ~nonce: int64, Uri.t) => Signature.t;};
