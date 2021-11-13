open Helpers;

[@deriving (ord, eq)]
type t =
  | Ed25519(Ed25519.Key.t)
  | Secp256k1(Secp256k1.Key.t);

let of_secret =
  fun
  | Secret.Ed25519(secret) => Ed25519(Ed25519.Key.of_secret(secret))
  | Secret.Secp256k1(secret) => Secp256k1(Secp256k1.Key.of_secret(secret));

let to_string =
  fun
  | Ed25519(key) => Ed25519.Key.to_string(key)
  | Secp256k1(key) => Secp256k1.Key.to_string(key);
let of_string = {
  let ed25519 = string => {
    let.some key = Ed25519.Key.of_string(string);
    Some(Ed25519(key));
  };
  let secp256k1 = string => {
    let.some key = Secp256k1.Key.of_string(string);
    Some(Secp256k1(key));
  };
  Encoding_helpers.parse_string_variant([ed25519, secp256k1]);
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
        | Ed25519(key) => Some(key)
        | _ => None,
        key =>
        Ed25519(key)
      ),
      case(
        Tag(1),
        Secp256k1.Key.encoding,
        ~title="Secp256k1",
        fun
        | Secp256k1(x) => Some(x)
        | _ => None,
        x =>
        Secp256k1(x)
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
