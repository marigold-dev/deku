open Crypto;
type t =
  | Ed25519(Ed25519.Secret.t);
let equal: (t, t) => bool;
let to_string: t => string;
let of_string: string => option(t);
