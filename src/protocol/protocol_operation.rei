open Crypto;

module Consensus: {
  [@deriving (eq, ord, yojson)]
  type t =
    | Add_validator(Validators.validator)
    | Remove_validator(Validators.validator);

  let sign: (Secret.t, t) => Signature.t;
  let verify: (Key.t, Signature.t, t) => bool;
};
module Core_tezos: {
  [@deriving (eq, ord, yojson)]
  type t = Core.Tezos_operation.t;
};
module Core_user: {
  [@deriving (eq, ord, yojson)]
  type t =
    pri {
      hash: BLAKE2B.t,
      key: Key.t,
      signature: Signature.t,
      nonce: int32,
      block_height: int64,
      data: Core.User_operation.t,
    };

  let sign:
    (
      ~secret: Secret.t,
      ~nonce: int32,
      ~block_height: int64,
      ~data: Core.User_operation.t
    ) =>
    t;
  /* TOOD: to be removed */
  let unsafe_make:
    (
      ~hash: BLAKE2B.t,
      ~key: Key.t,
      ~signature: Signature.t,
      ~nonce: int32,
      ~block_height: int64,
      ~data: Core.User_operation.t
    ) =>
    t;
};
[@deriving (eq, ord, yojson)]
type t =
  | Core_tezos(Core.Tezos_operation.t)
  | Core_user(Core_user.t)
  | Consensus(Consensus.t);
