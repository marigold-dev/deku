[@deriving (ord, eq)]
type t =
  | Ed25519(Ed25519.Key_hash.t)
  | Secp256k1(Secp256k1.Key_hash.t)
  | P256(P256.Key_hash.t);

let of_key: Key.t => t;

let encoding: Data_encoding.t(t);
let to_string: t => string;
let of_string: string => option(t);

let to_yojson: t => Yojson.Safe.t;
let of_yojson: Yojson.Safe.t => result(t, string);
