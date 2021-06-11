module Key: {
  type t =
    | Ed25519(Mirage_crypto_ec.Ed25519.pub_);

  let encoding: Data_encoding.t(t);
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
