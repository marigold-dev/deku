[@deriving (ord, eq)]
type t =
  | Ed25519(Ed25519.Key.t)
  | Secp256k1(Secp256k1.Key.t);

let of_secret: Secret.t => t;

let encoding: Data_encoding.t(t);
let to_string: t => string;
let of_string: string => option(t);

let to_yojson: t => Yojson.Safe.t;
let of_yojson: Yojson.Safe.t => result(t, string);
