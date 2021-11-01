[@deriving (ord, eq)]
type t =
  | Ed25519(Ed25519.Signature.t)
  | Secp256k1(Secp256k1.Signature.t);

let sign: (Secret.t, string) => t;
let verify: (Key.t, t, string) => bool;

let to_string: t => string;
let of_string: string => option(t);

let to_yojson: t => Yojson.Safe.t;
let of_yojson: Yojson.Safe.t => result(t, string);
