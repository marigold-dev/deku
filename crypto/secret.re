open Helpers;
[@deriving (ord, eq)]
type t =
  | Ed25519(Ed25519.Secret.t);

let to_string =
  fun
  | Ed25519(secret) => Ed25519.Secret.to_string(secret);
let of_string = {
  let ed25519 = string => {
    let.some secret = Ed25519.Secret.of_string(string);
    Some(Ed25519(secret));
  };
  Encoding_helpers.parse_string_variant([ed25519]);
};

let encoding = {
  open Data_encoding;
  let name = "Signature.Secret_key";
  let title = "A Ed25519, Secp256k1 or P256 secret key";
  let raw_encoding =
    def("secret_key", ~description=title) @@
    union([
      case(
        Tag(0),
        Ed25519.Secret.encoding,
        ~title="Ed25519",
        fun
        | Ed25519(secret) => Some(secret),
        secret =>
        Ed25519(secret)
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
  Yojson_ext.with_yojson_string("secret", to_string, of_string);
