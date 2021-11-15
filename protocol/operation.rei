open Crypto;
module Main_chain: {
  [@deriving yojson]
  type kind =
    // TODO: can a validator uses the same key in different nodes?
    // If so the ordering in the list must never use the same key two times in sequence
    | Deposit({
        destination: Address.t,
        amount: Amount.t,
        ticket: Ticket.t,
      });

  [@deriving (ord, yojson)]
  type t =
    pri {
      hash: BLAKE2B.t,
      tezos_hash: Tezos.Operation_hash.t,
      tezos_index: int,
      kind,
    };

  let make:
    (~tezos_hash: Tezos.Operation_hash.t, ~tezos_index: int, ~kind: kind) => t;
  let verify:
    (
      ~hash: BLAKE2B.t,
      ~tezos_hash: Tezos.Operation_hash.t,
      ~tezos_index: int,
      ~kind: kind
    ) =>
    result(t, string);
};

module Side_chain: {
  // TODO: I don't like this structure model
  [@deriving yojson]
  type kind =
    | Transaction({
        destination: Address.t,
        amount: Amount.t,
        ticket: Ticket.t,
      })
    | Withdraw({
        owner: Tezos.Address.t,
        amount: Amount.t,
        ticket: Ticket.t,
      })
    | Add_validator(Validators.validator)
    | Remove_validator(Validators.validator);

  [@deriving (ord, yojson)]
  type t =
    pri {
      hash: BLAKE2B.t,
      signature: Protocol_signature.t,
      nonce: int32,
      block_height: int64,
      source: Address.t,
      kind,
    };

  let sign:
    (
      ~secret: Secret.t,
      ~nonce: int32,
      ~block_height: int64,
      ~source: Address.t,
      ~kind: kind
    ) =>
    t;

  let verify:
    (
      ~hash: BLAKE2B.t,
      ~signature: Protocol_signature.t,
      ~nonce: int32,
      ~block_height: int64,
      ~source: Address.t,
      ~kind: kind
    ) =>
    result(t, string);
};
