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
    let sign: (Secret.t, string) => t;
    let check: (Key.t, t, string) => bool;
    let to_string: t => string;
    let of_string: string => option(t);
  };
};
module Secp256k1 = Libsecp256k1.External;
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
  module Secp256k1:
    S with
      type Key.t = Secp256k1.Key.t(Secp256k1.Key.public) and
      type Key_hash.t = BLAKE2B_20.t and
      type Secret.t = Secp256k1.Key.t(Secp256k1.Key.secret) and
      type Signature.t = Secp256k1.Sign.t(Secp256k1.Sign.plain);
};
