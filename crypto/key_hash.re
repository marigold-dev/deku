open Helpers;
[@deriving (ord, eq)]
type t =
  | Ed25519(Ed25519.Key_hash.t);

let of_key = (Key.Ed25519(key)) => Ed25519(Ed25519.Key_hash.of_key(key));
let to_string =
  fun
  | Ed25519(key_hash) => Ed25519.Key_hash.to_string(key_hash);
let of_string = {
  let ed25519 = string => {
    let.some key_hash = Ed25519.Key_hash.of_string(string);
    Some(Ed25519(key_hash));
  };
  Encoding_helpers.parse_string_variant([ed25519]);
};

let encoding = {
  open Data_encoding;
  let name = "Signature.Public_key_hash";
  let title = "A Ed25519, Secp256k1, or P256 public key hash";
  let raw_encoding =
    def("public_key_hash", ~description=title) @@
    union([
      case(
        Tag(0),
        Ed25519.Key_hash.encoding,
        ~title="Ed25519",
        fun
        | Ed25519(key_hash) => Some(key_hash),
        key_hash =>
        Ed25519(key_hash)
      ),
    ]);
  Encoding_helpers.make_encoding(
    ~name,
    ~title,
    ~to_string,
    ~of_string,
    ~raw_encoding,
  );
};

let (to_yojson, of_yojson) =
  Yojson_ext.with_yojson_string("key_hash", to_string, of_string);
