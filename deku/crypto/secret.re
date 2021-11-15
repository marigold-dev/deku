open Helpers;
[@deriving (ord, eq)]
type t =
  | Ed25519(Ed25519.Secret.t)
  | Secp256k1(Secp256k1.Secret.t);

let to_string =
  fun
  | Ed25519(secret) => Ed25519.Secret.to_string(secret)
  | Secp256k1(secret) => Secp256k1.Secret.to_string(secret);
let of_string = {
  let ed25519 = string => {
    let.some secret = Ed25519.Secret.of_string(string);
    Some(Ed25519(secret));
  };
  let secp256k1 = string => {
    let.some secret = Secp256k1.Secret.of_string(string);
    Some(Secp256k1(secret));
  };
  Encoding_helpers.parse_string_variant([ed25519, secp256k1]);
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
        | Ed25519(secret) => Some(secret)
        | _ => None,
        secret =>
        Ed25519(secret)
      ),
      case(
        Tag(1),
        Secp256k1.Secret.encoding,
        ~title="Secp256k1",
        fun
        | Secp256k1(secret) => Some(secret)
        | _ => None,
        secret =>
        Secp256k1(secret)
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
