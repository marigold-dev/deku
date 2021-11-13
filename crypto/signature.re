open Helpers;
[@deriving (ord, eq)]
type t =
  | Ed25519(Ed25519.Signature.t)
  | Secp256k1(Secp256k1.Signature.t);

let sign = (secret, hash) =>
  switch (secret) {
  | Secret.Ed25519(secret) => Ed25519(Ed25519.sign(secret, hash))
  | Secret.Secp256k1(secret) => Secp256k1(Secp256k1.sign(secret, hash))
  };
let verify = (key, signature, hash) =>
  switch (key, signature) {
  | (Key.Ed25519(key), Ed25519(signature)) =>
    Ed25519.verify(key, signature, hash)
  | (Key.Secp256k1(key), Secp256k1(signature)) =>
    Secp256k1.verify(key, signature, hash)
  | (Key.Ed25519(_) | Key.Secp256k1(_), Ed25519(_) | Secp256k1(_)) => false
  };

let to_string =
  fun
  | Ed25519(signature) => Ed25519.Signature.to_string(signature)
  | Secp256k1(signature) => Secp256k1.Signature.to_string(signature);
let of_string = {
  let ed25519 = string => {
    let.some signature = Ed25519.Signature.of_string(string);
    Some(Ed25519(signature));
  };
  let secp256k1 = string => {
    let.some signature = Secp256k1.Signature.of_string(string);
    Some(Secp256k1(signature));
  };
  Encoding_helpers.parse_string_variant([ed25519, secp256k1]);
};

let (to_yojson, of_yojson) =
  Yojson_ext.with_yojson_string("signature", to_string, of_string);
