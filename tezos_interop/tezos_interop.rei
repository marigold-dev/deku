module Key: {
  type t =
    | Ed25519(Mirage_crypto_ec.Ed25519.pub_);

  let encoding: Data_encoding.t(t);
  let to_string: t => string;
  let of_string: string => option(t);
};
