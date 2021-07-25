open Helpers;
module Main_chain: {
  [@deriving (ord, yojson)]
  type kind =
    // TODO: can a validator uses the same key in different nodes?
    // If so the ordering in the list must never use the same key two times in sequence
    | Add_validator(Validators.validator)
    | Remove_validator(Validators.validator);

  [@deriving (ord, yojson)]
  type t =
    pri {
      tezos_hash: BLAKE2B.t,
      kind,
    };

  let make: (~tezos_hash: BLAKE2B.t, ~kind: kind) => t;
};

module Side_chain: {
  // TODO: I don't like this structure model
  [@deriving (ord, yojson)]
  type kind =
    | Transaction({destination: Wallet.t});
  [@deriving (ord, yojson)]
  type t =
    pri {
      hash: BLAKE2B.t,
      signature: Signature.t,
      nonce: int32,
      block_height: int64,
      source: Wallet.t,
      amount: Amount.t,
      kind,
    };

  let sign:
    (
      ~secret: Address.key,
      ~nonce: int32,
      ~block_height: int64,
      ~source: Wallet.t,
      ~amount: Amount.t,
      ~kind: kind
    ) =>
    t;

  let verify:
    (
      ~hash: BLAKE2B.t,
      ~signature: Signature.t,
      ~nonce: int32,
      ~block_height: int64,
      ~source: Wallet.t,
      ~amount: Amount.t,
      ~kind: kind
    ) =>
    result(t, string);
};
