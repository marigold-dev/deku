open Helpers;
type t = BLAKE2B_20.t;
let equal: (t, t) => bool;
let encoding: Data_encoding.t(t);
let to_string: t => string;
let of_string: string => option(t);
