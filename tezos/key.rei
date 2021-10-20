open Crypto;

type t =
  | Ed25519(Ed25519.Key.t);
let encoding: Data_encoding.t(t);
let compare: (t, t) => int;
let of_secret: Secret.t => t;
let equal: (t, t) => bool;
let to_string: t => string;
let of_string: string => option(t);
