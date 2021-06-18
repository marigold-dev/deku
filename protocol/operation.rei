open Helpers;
module Main_chain: {
  [@deriving (ord, yojson)]
  type t =
    | Deposit({
        destination: Wallet.t,
        amount: Amount.t,
      })
    | Withdraw({
        source: Wallet.t,
        amount: Amount.t,
      })
    // TODO: can a validator uses the same key in different nodes?
    // If so the ordering in the list must never use the same key two times in sequence
    | Add_validator(Validators.validator)
    | Remove_validator(Validators.validator);
};

module Side_chain: {
  // TODO: I don't like this structure model
  [@deriving (ord, yojson)]
  type kind =
    | Transaction({destination: Wallet.t})
    | Freeze
    | Unfreeze;
  [@deriving (ord, yojson)]
  type t =
    pri {
      hash: BLAKE2B.t,
      nonce: int32,
      block_height: int64,
      source: Wallet.t,
      amount: Amount.t,
      kind,
    };

  let make:
    (
      ~nonce: int32,
      ~block_height: int64,
      ~source: Wallet.t,
      ~amount: Amount.t,
      ~kind: kind
    ) =>
    t;

  // TODO: maybe use GADT for this?
  module Self_signed: Signed.S with type data = t;
};

let self_sign_side:
  (~key: Address.key, Side_chain.t) => Side_chain.Self_signed.t;
