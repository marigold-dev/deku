open Helpers;
open Crypto;
[@deriving (eq, ord)]
type t =
  | Ed25519(Ed25519.Key_hash.t);

let name = "Signature.Public_key_hash";

let title = "A Ed25519, Secp256k1, or P256 public key hash";
let encoding = {
  open Data_encoding;
  let raw_encoding =
    def("public_key_hash", ~description=title) @@
    union([
      case(
        Tag(0),
        Ed25519.Key_hash.encoding,
        ~title="Ed25519",
        fun
        | Ed25519(x) => Some(x),
        x =>
        Ed25519(x)
      ),
    ]);
  obj1(req(name, raw_encoding));
};
let of_key = t =>
  switch (t) {
  | Key.Ed25519(pub_) => Ed25519(Ed25519.Key_hash.hash_key(pub_))
  };
let to_string =
  fun
  | Ed25519(hash) => Ed25519.Key_hash.to_string(hash);
let of_string = {
  let ed25519 = string => {
    let.some key = Ed25519.Key_hash.of_string(string);
    Some(Ed25519(key));
  };
  List.try_decode_list([ed25519]);
};
