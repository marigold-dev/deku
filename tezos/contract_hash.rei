open Crypto;

[@deriving (eq, ord, yojson)]
type t = BLAKE2B_20.t;
let encoding: Data_encoding.t(t);
let to_string: t => string;
let of_string: string => option(t);
