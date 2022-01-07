[@deriving (ord, eq)]
type t =
  | Ed25519(Ed25519.Secret.t)
  | Secp256k1(Secp256k1.Secret.t)
  | P256(P256.Secret.t);

let encoding: Data_encoding.t(t);
let to_string: t => string;
let of_string: string => option(t);

let to_yojson: t => Yojson.Safe.t;
let of_yojson: Yojson.Safe.t => result(t, string);
