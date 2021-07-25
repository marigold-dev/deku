open Helpers;

module Base58 = Base58;

module Key: {
  type t =
    | Ed25519(Mirage_crypto_ec.Ed25519.pub_);

  let to_string: t => string;
  let of_string: string => option(t);
};

module Key_hash: {
  type t =
    | Ed25519(Helpers.BLAKE2B_20.t);

  let of_key: Key.t => t;

  let to_string: t => string;
  let of_string: string => option(t);
};

module Secret: {
  type t =
    | Ed25519(Mirage_crypto_ec.Ed25519.priv);

  let to_string: t => string;
  let of_string: string => option(t);
};

module Signature: {
  type t = pri | Ed25519(string);

  let sign: (Secret.t, string) => t;
  let check: (Key.t, t, string) => bool;

  let to_string: t => string;
  let of_string: string => option(t);
};

module Contract_hash: {
  type t = BLAKE2B_20.t;

  let to_string: t => string;
  let of_string: string => option(t);
};

module Address: {
  type t =
    | Implicit(Key_hash.t)
    | Originated(Contract_hash.t);

  let to_string: t => string;
  let of_string: string => option(t);
};

module Ticket: {
  type t = {
    ticketer: Address.t,
    data: bytes,
  };

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

module Consensus: {
  let hash_validators: list(Key.t) => BLAKE2B.t;
  let hash_block:
    (
      ~block_height: int64,
      ~block_payload_hash: BLAKE2B.t,
      ~state_root_hash: BLAKE2B.t,
      ~validators_hash: BLAKE2B.t
    ) =>
    BLAKE2B.t;
};

module Discovery: {let sign: (Secret.t, ~nonce: int64, Uri.t) => Signature.t;};
