open Helpers;
open Crypto;

[@deriving eq]
type t =
  | Ed25519(Ed25519.Key.t);

let name = "Signature.Public_key";
let title = "A Ed25519, Secp256k1, or P256 public key";
let encoding = {
  open Data_encoding;
  let raw_encoding =
    def("public_key", ~description=title) @@
    union([
      case(
        Tag(0),
        Ed25519.Key.encoding,
        ~title="Ed25519",
        fun
        | Ed25519(x) => Some(x),
        x =>
        Ed25519(x)
      ),
    ]);

  // TODO: move this to a functor
  obj1(req(name, raw_encoding));
};

let to_string =
  fun
  | Ed25519(key) => Ed25519.Key.to_string(key);
let of_string = {
  let ed25519 = string => {
    let.some key = Ed25519.Key.of_string(string);
    Some(Ed25519(key));
  };
  List.try_decode_list([ed25519]);
};
