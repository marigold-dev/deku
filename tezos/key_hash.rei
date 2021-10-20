open Helpers;

type t =
  | Ed25519(BLAKE2B_20.t);
let encoding: Data_encoding.t(t);
let of_key: Key.t => t;
let equal: (t, t) => bool;
let to_string: t => string;
let of_string: string => option(t);
