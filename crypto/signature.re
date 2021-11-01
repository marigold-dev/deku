open Helpers;
[@deriving (ord, eq)]
type t =
  | Ed25519(Ed25519.Signature.t);

let sign = (secret, message) =>
  switch (secret) {
  | Secret.Ed25519(secret) => Ed25519(Ed25519.sign(secret, message))
  };
let verify = (key, signature, message) =>
  switch (key, signature) {
  | (Key.Ed25519(key), Ed25519(signature)) =>
    Ed25519.verify(key, signature, message)
  };

let to_string =
  fun
  | Ed25519(signature) => Ed25519.Signature.to_string(signature);
let of_string = {
  let ed25519 = string => {
    let.some signature = Ed25519.Signature.of_string(string);
    Some(Ed25519(signature));
  };
  Encoding_helpers.parse_string_variant([ed25519]);
};

let (to_yojson, of_yojson) =
  Yojson_ext.with_yojson_string("signature", to_string, of_string);
