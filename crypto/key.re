open Helpers;

[@deriving (ord, eq)]
type t =
  | Ed25519(Ed25519.Key.t);

let to_string =
  fun
  | Ed25519(key) => Ed25519.Key.to_string(key);
let of_string = {
  let ed25519 = string => {
    let.some key = Ed25519.Key.of_string(string);
    Some(Ed25519(key));
  };
  Encoding_helpers.parse_string_variant([ed25519]);
};

let encoding = {
  open Data_encoding;

  let name = "Signature.Public_key";
  let title = "A Ed25519, Secp256k1, or P256 public key";
  let raw_encoding =
    def("public_key", ~description=title) @@
    union([
      case(
        Tag(0),
        Ed25519.Key.encoding,
        ~title="Ed25519",
        fun
        | Ed25519(key) => Some(key),
        key =>
        Ed25519(key)
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
  Yojson_ext.with_yojson_string("key", to_string, of_string);
