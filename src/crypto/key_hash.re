open Helpers;

[@deriving (ord, eq)]
type t =
  | Ed25519(Ed25519.Key_hash.t)
  | Secp256k1(Secp256k1.Key_hash.t)
  | P256(P256.Key_hash.t);

let of_key =
  fun
  | Key.Ed25519(key) => Ed25519(Ed25519.Key_hash.of_key(key))
  | Key.Secp256k1(key) => Secp256k1(Secp256k1.Key_hash.of_key(key))
  | Key.P256(key) => P256(P256.Key_hash.of_key(key));
let matches_key = (key, t) => equal(of_key(key), t);

let make_ed25519 = () => {
  let (secret, key) = Ed25519.generate();
  let key_hash = Ed25519.Key_hash.of_key(key);
  (Secret.Ed25519(secret), Key.Ed25519(key), Ed25519(key_hash));
};

let to_string =
  fun
  | Ed25519(key_hash) => Ed25519.Key_hash.to_string(key_hash)
  | Secp256k1(key_hash) => Secp256k1.Key_hash.to_string(key_hash)
  | P256(key_hash) => P256.Key_hash.to_string(key_hash);
let of_string = {
  let ed25519 = string => {
    let.some key_hash = Ed25519.Key_hash.of_string(string);
    Some(Ed25519(key_hash));
  };
  let secp256k1 = string => {
    let.some key = Secp256k1.Key_hash.of_string(string);
    Some(Secp256k1(key));
  };
  let p256 = string => {
    let.some key = P256.Key_hash.of_string(string);
    Some(P256(key));
  };
  Encoding_helpers.parse_string_variant([ed25519, secp256k1, p256]);
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
        | Ed25519(key_hash) => Some(key_hash)
        | _ => None,
        key_hash =>
        Ed25519(key_hash)
      ),
      case(
        Tag(1),
        Secp256k1.Key_hash.encoding,
        ~title="Secp256k1",
        fun
        | Secp256k1(x) => Some(x)
        | _ => None,
        x =>
        Secp256k1(x)
      ),
      case(
        Tag(2),
        P256.Key_hash.encoding,
        ~title="P256",
        fun
        | P256(x) => Some(x)
        | _ => None,
        x =>
        P256(x)
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
