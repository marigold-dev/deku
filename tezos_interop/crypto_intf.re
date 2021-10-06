module type S = {
  module Key: {
    type t;
    let encoding: Data_encoding.t(t);
    let equal: (t, t) => bool;
    let to_string: t => string;
    let of_string: string => option(t);
  };
  module Key_hash: {
    type t;
    let hash_key: Key.t => t;
    let encoding: Data_encoding.t(t);
    let equal: (t, t) => bool;
    let to_string: t => string;
    let of_string: string => option(t);
  };
  module Secret: {
    type t;
    let equal: (t, t) => bool;

    let to_string: t => string;
    let of_string: string => option(t);
  };
  module Signature: {
    type t;
    let equal: (t, t) => bool;
    let sign: (Secret.t, t) => t;
    let check: (Key.t, t, t) => bool;
    let to_string: t => t;
    let of_string: t => option(t);
  };
};
module type Intf = {
  open Helpers;
  open Mirage_crypto_ec;
  module type S = S;
  module Ed25519:
    S with
      type Key.t = Ed25519.pub_ and
      type Key_hash.t = BLAKE2B_20.t and
      type Secret.t = Ed25519.priv and
      type Signature.t = string;
  module P256:
    S with
      type Key.t = P256.Dsa.pub_ and
      type Key_hash.t = BLAKE2B_20.t and
      type Secret.t = P256.Dsa.priv and
      type Signature.t = string;
};
