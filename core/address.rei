open Crypto;

[@deriving (eq, ord, yojson)]
type t;

let matches_key: (Key.t, t) => bool;

let make: unit => (Secret.t, t);

let of_key: Key.t => t;

let to_key_hash: t => Key_hash.t;
let of_key_hash: Key_hash.t => t;

let to_string: t => string;
let of_string: string => option(t);
