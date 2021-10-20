open Crypto;
type t =
  | Ed25519(Ed25519.Signature.t);

let equal: (t, t) => bool;
let compare: (t, t) => int;
let sign: (Secret.t, string) => t;
let check: (Key.t, t, string) => bool;

let to_string: t => string;
let of_string: string => option(t);
